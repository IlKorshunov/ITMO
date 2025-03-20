#pragma once
#include "function_models.h"

template <typename F>
class function;

template <typename R, typename... Args>
class function<R(Args...)> {
private:
  template <typename F = void>
  using model_t = detail::modeel_t<F, R, Args...>;

  template <typename F = void>
  static constexpr model_t<F> _model{};

private:
  const detail::model_concept<R, Args...>* _descriptor;
  detail::target_storage_t _storage;

public:
  function() noexcept : _descriptor(&_model<>), _storage{} {}

  template <typename F>
  function(F func) : _descriptor(&_model<F>),
                     _storage{} {
    model_t<F>::construct(_storage, std::move(func));
  }

  function(const function& other) : _descriptor(other._descriptor) {
    other._descriptor->copy_construct(_storage, other._storage);
  }

  function(function&& other) noexcept : _descriptor(std::exchange(other._descriptor, &_model<>)) {
    _descriptor->move_construct(_storage, other._storage);
  }

  function& operator=(const function& other) {
    if (&other != this) {
      *this = function(other);
    }
    return *this;
  }

  function& operator=(function&& other) noexcept {
    if (&other != this) {
      _descriptor->destroy(_storage);
      _descriptor = std::exchange(other._descriptor, &_model<>);
      _descriptor->move_construct(_storage, other._storage);
    }
    return *this;
  }

  ~function() {
    _descriptor->destroy(_storage);
  }

  explicit operator bool() const noexcept {
    return _descriptor != &_model<>;
  }

  R operator()(Args... args) const {
    return _descriptor->call(const_cast<detail::target_storage_t&>(_storage), std::forward<Args>(args)...);
  }

  template <typename T>
  T* target() noexcept {
    if (_descriptor == &_model<T>) {
      return &model_t<T>::target(_storage);
    } else {
      return nullptr;
    }
  }

  template <typename T>
  const T* target() const noexcept {
    if (_descriptor == &_model<T>) {
      return &model_t<T>::target(_storage);
    } else {
      return nullptr;
    }
  }
};