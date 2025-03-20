#pragma once

#include <cstddef>
#include <utility>
#include "control_blocks.h"

template <typename T>
class shared_ptr {
  template <typename U>
  friend class shared_ptr;
  template <typename U>
  friend class weak_ptr;
  template <typename U, typename... Args>
  friend shared_ptr<U> make_shared(Args&&... args);

public:
  shared_ptr() noexcept : cb(nullptr), ptr(nullptr) {}

  shared_ptr(std::nullptr_t) noexcept : shared_ptr() {}

  ~shared_ptr() noexcept { _decrease_strong_ref(); }

  template <typename Y>
  explicit shared_ptr(Y* ptr) : shared_ptr(ptr, std::default_delete<Y>()) {}

  template <typename Y, typename Deleter>
  shared_ptr(Y* ptr, Deleter deleter) : ptr(ptr) {
    try {
      cb = new detail::control_block_ptr_t(ptr, std::move(deleter));
    } catch (...) {
      deleter(ptr);
      throw;
    }
  }

  template <typename Y>
  shared_ptr(const shared_ptr<Y>& other, T* ptr) noexcept : cb(other.cb), ptr(ptr) { _increase_strong_ref(); }

  template <typename Y>
  shared_ptr(shared_ptr<Y>&& other, T* ptr) noexcept : cb(std::exchange(other.cb, nullptr)), ptr(ptr) {other.ptr=nullptr;}

  shared_ptr(const shared_ptr& other) noexcept : cb(other.cb), ptr(other.ptr) { _increase_strong_ref(); }

  template <typename Y, typename = std::enable_if_t<std::is_convertible_v<Y*, T*>>>
  shared_ptr(const shared_ptr<Y>& other) noexcept : shared_ptr(other, other.get()) {}

  shared_ptr(shared_ptr&& other) noexcept : shared_ptr(std::move(other), other.get()) {}

  template <typename Y, typename = std::enable_if_t<std::is_convertible_v<Y*, T*>>>
  shared_ptr(shared_ptr<Y>&& other) noexcept : shared_ptr(std::move(other), other.get()) {}



  shared_ptr& operator=(const shared_ptr& other) noexcept {
    if (&other != this) {
      shared_ptr temp(other);
      _swap(*this, temp);
    }
    return *this;
  }

  template <typename Y>
  shared_ptr& operator=(const shared_ptr<Y>& other) noexcept {
    shared_ptr temp(other);
    _swap(*this, temp);
    return *this;
  }

shared_ptr& operator=(shared_ptr&& other) noexcept {
  if (this != &other) {
    reset();  
    ptr = std::exchange(other.ptr, nullptr);
    cb  = std::exchange(other.cb, nullptr);
  }
  return *this;
}


  template <typename Y>
  shared_ptr& operator=(shared_ptr<Y>&& other) noexcept {
    reset();  
    ptr = std::exchange(other.ptr, nullptr);
    cb  = std::exchange(other.cb, nullptr);
    return *this;
  }

  T* get() const noexcept {
    return ptr;
  }

  operator bool() const noexcept {
    return ptr != nullptr;
  }

  T& operator*() const noexcept {
    return *get();
  }

  T* operator->() const noexcept {
    return get();
  }

  std::size_t use_count() const noexcept {
    return cb ? cb->strong_ref : 0;
  }

  void reset() noexcept {
    _decrease_strong_ref();
    cb = nullptr;
    ptr = nullptr;
  }

  template <typename Y>
  void reset(Y* new_ptr) {
    shared_ptr tmp(new_ptr);
    _swap(tmp, *this);
  }

  template <typename Y, typename Deleter>
  void reset(Y* new_ptr, Deleter deleter) {
    shared_ptr tmp(new_ptr, deleter);
    _swap(tmp, *this);
  }

  friend bool operator==(const shared_ptr& lhs, const shared_ptr& rhs) noexcept {
    return lhs.ptr == rhs.ptr;
  }

  friend bool operator!=(const shared_ptr& lhs, const shared_ptr& rhs) noexcept {
    return !operator==(lhs, rhs);
  }


private:
  detail::control_block_t* cb;
  T* ptr;

  shared_ptr(detail::control_block_t* cb, T* ptr) noexcept : cb(cb), ptr(ptr) {}

  void  _increase_strong_ref() {if (cb) cb->inc_strong_ref();}

  void _decrease_strong_ref() {if (cb) cb->dec_strong_ref();}

  void _swap(shared_ptr& lhs, shared_ptr& rhs) noexcept {
    std::swap(lhs.cb, rhs.cb);
    std::swap(lhs.ptr, rhs.ptr);
  }
};

template <typename T>
class weak_ptr {
  template <typename U>
  friend class weak_ptr;
  template <typename U>
  friend class shared_ptr;

public:
  weak_ptr() noexcept : cb(nullptr), ptr(nullptr) {}

  ~weak_ptr() { _dec_weak_refs(); }

  template <typename Y>
  weak_ptr(const shared_ptr<Y>& other) noexcept : cb(other.cb), ptr(other.ptr) { _increase_weak_ptr(); }

  weak_ptr(const weak_ptr& other) noexcept : cb(other.cb), ptr(other.ptr) { _increase_weak_ptr(); }

  template <typename Y>
  weak_ptr(const weak_ptr<Y>& other) noexcept : cb(other.cb), ptr(other.ptr) { _increase_weak_ptr(); }

  weak_ptr(weak_ptr&& other) noexcept : cb(std::exchange(other.cb, nullptr)), ptr(std::exchange(other.ptr, nullptr)) {}

  template <typename Y>
  weak_ptr(weak_ptr<Y>&& other) noexcept : cb(std::exchange(other.cb, nullptr)), ptr(std::exchange(other.ptr, nullptr)) {}

  template <typename Y>
  weak_ptr& operator=(const shared_ptr<Y>& other) noexcept {
    weak_ptr tmp(other);
    _swap(*this, tmp);
    return *this;
  }

  weak_ptr& operator=(const weak_ptr& other) noexcept {
    if (&other != this) {
      weak_ptr tmp(std::move(other));
      _swap(*this, tmp);
    }
    return *this;
  }

  template <typename Y>
  weak_ptr& operator=(const weak_ptr<Y>& other) noexcept {
    weak_ptr tmp(other);
    _swap(*this, tmp);
    return *this;
  }

  weak_ptr& operator=(weak_ptr&& other) noexcept {
    if (&other != this) {
      weak_ptr tmp(std::move(other));
      _swap(*this, tmp);
    }
    return *this;
  }

  template <typename Y>
  weak_ptr& operator=(weak_ptr<Y>&& other) noexcept {
    weak_ptr tmp(std::move(other));
    _swap(*this, tmp);
    return *this;
  }

  shared_ptr<T> lock() const noexcept {
    if (!expired()){
      cb->strong_ref++;
      return shared_ptr<T>(cb, ptr);
    } else {
      return shared_ptr<T>();
    }
  }

  bool expired() const noexcept {
    return !cb || cb->strong_ref == 0;
  }

  void reset() noexcept {
    _dec_weak_refs();
    cb = nullptr;
    ptr = nullptr;
  }

  friend bool operator==(const weak_ptr& lhs, const weak_ptr& rhs) noexcept {
    return lhs.cb == rhs.cb && lhs.ptr == rhs.ptr;
  }

  friend bool operator!=(const weak_ptr& lhs, const weak_ptr& rhs) noexcept {
    return !(lhs == rhs);
  }

private:
  detail::control_block_t* cb;
  T* ptr;

  void _increase_weak_ptr() { if (cb) cb->inc_weak_ref(); }

  void _dec_weak_refs() noexcept { if (cb) cb->dec_weak();}

  void _swap(weak_ptr& lhs, weak_ptr& rhs) noexcept {
    std::swap(lhs.cb, rhs.cb);
    std::swap(lhs.ptr, rhs.ptr);
  }
};

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args&&... args) {
  auto* cb = new detail::control_block_object_t<T>(std::forward<Args>(args)...);
  return shared_ptr<T>(static_cast<detail::control_block_t*>(cb), cb->get_data());
}