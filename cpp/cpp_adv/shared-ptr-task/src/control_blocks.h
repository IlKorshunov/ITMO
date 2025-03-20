#pragma once

#include <cstddef>
#include <memory>

namespace detail {

struct control_block_t {
  size_t strong_ref = 1;
  size_t weak_ref = 0;

  virtual ~control_block_t() = default;

  void dec_strong_ref() {
    strong_ref--;
    if (strong_ref == 0) clear();
    check_delete();
  }

  void dec_weak() {
    --weak_ref;
    check_delete();
  }

  void inc_strong_ref() { ++strong_ref; }
  void inc_weak_ref() { ++weak_ref; }

  virtual void clear() = 0;

  void check_delete() {
    if (strong_ref == 0 && weak_ref == 0) { delete this; }
  }

};

template <typename T>
struct control_block_object_t : control_block_t {
  alignas(T) std::byte storage[sizeof(T)];

  template <typename... Args>
  control_block_object_t(Args&&... args) {
      new (storage) T(std::forward<Args>(args)...);
  }

  T* get_data() { return reinterpret_cast<T*>(storage); }

  void clear() {
    get_data()->~T();
  }

};

template <typename T, typename Deleter>
struct control_block_ptr_t : control_block_t {
  T* ptr;
  [[no_unique_address]] Deleter deleter;

  control_block_ptr_t(T* ptr, Deleter&& deleter) noexcept : ptr(ptr), deleter(std::move(deleter)) {}
  ~control_block_ptr_t() = default;
  void clear() noexcept override { deleter(ptr); }
};

} // namespace detail