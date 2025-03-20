#pragma once
#include "bad_function_call.h"

#include <memory>
#include <utility>
#include <cstddef> 

namespace detail {
using target_storage_t = std::aligned_storage_t<sizeof(std::max_align_t), alignof(std::max_align_t)>;

template <typename F>
concept small_function_object = sizeof(F) <= sizeof(std::max_align_t) && std::is_nothrow_move_constructible_v<F>;

template <typename R, typename... Args>
struct model_concept {
  virtual void copy_construct(target_storage_t& to, const target_storage_t& from) const = 0;
  virtual void move_construct(target_storage_t& to, target_storage_t& from) const noexcept = 0;
  virtual R call(target_storage_t& storage, Args... args) const = 0;
  virtual void destroy(target_storage_t& storage) const noexcept = 0;
};

template <typename F, typename R, typename... Args>
struct model_t;

template <typename R, typename... Args>
struct model_t<void, R, Args...> final : model_concept<R, Args...> {
  void copy_construct(target_storage_t&, const target_storage_t&) const override {}

  void move_construct(target_storage_t&, target_storage_t&) const noexcept override {}

  R call(target_storage_t&, Args...) const override {
    throw bad_function_call();
  }

  void destroy(target_storage_t&) const noexcept override {}
};

template <small_function_object F, typename R, typename... Args>
struct model_t<F, R, Args...> final : model_concept<R, Args...> {
  static void construct(target_storage_t& storage, F&& func) {
    new (&storage) F(std::move(func));
  }

  static F& target(target_storage_t& storage) {
    return *std::launder(reinterpret_cast<F*>(&storage));
  }

  static const F& target(const target_storage_t& storage) {
    return *std::launder(reinterpret_cast<const F*>(&storage));
  }

  void copy_construct(target_storage_t& to, const target_storage_t& from) const override {
    new (&to) F(target(from));
  }

  void move_construct(target_storage_t& to, target_storage_t& from) const noexcept override {
    new (&to) F(std::move(target(from)));
    destroy(from);
  }

  R call(target_storage_t& storage, Args... args) const override {
    return target(storage)(std::forward<Args>(args)...);
  }

  void destroy(target_storage_t& storage) const noexcept override {
    target(storage).~F();
  }
};

template <typename F, typename R, typename... Args>
struct model_t final : model_concept<R, Args...> {
  using target_pointer = target_storage_t;

  static void construct(target_pointer& pointer, F&& func) {
    new (&pointer) F*(new F(std::move(func)));
  }

  static F& target(target_pointer& pointer) {
    return *std::launder(reinterpret_cast<F*&>(pointer));
  }

  static const F& target(const target_pointer& pointer) {
    return *std::launder(reinterpret_cast<const F* const&>(pointer));
  }

  void copy_construct(target_pointer& to, const target_pointer& from) const override {
    new (&to) F*(new F(target(from)));
  }

  void move_construct(target_pointer& to, target_pointer& from) const noexcept override {
    to = from;
  }

  R call(target_pointer& pointer, Args... args) const override {
    return target(pointer)(std::forward<Args>(args)...);
  }

  void destroy(target_pointer& pointer) const noexcept override {
    delete &target(pointer);
  }
};
} // namespace detail