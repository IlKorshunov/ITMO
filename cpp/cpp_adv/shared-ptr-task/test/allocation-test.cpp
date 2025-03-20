#include "shared-ptr.h"
#include "test-classes.h"

#include <gtest/gtest.h>

#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define DISABLE_ALLOCATION_TESTS 1
#endif
#endif

#ifndef DISABLE_ALLOCATION_TESTS

namespace {

struct injected_fault : std::runtime_error {
  using runtime_error::runtime_error;
};

bool should_inject_fault();

void* injected_allocate(size_t count);
void injected_deallocate(void* ptr);

struct fault_injection_disable {
  fault_injection_disable();

  fault_injection_disable(const fault_injection_disable&) = delete;
  fault_injection_disable& operator=(const fault_injection_disable&) = delete;

  ~fault_injection_disable();

private:
  bool was_disabled;
};

template <typename T>
struct fault_injection_allocator {
  using value_type = T;

  fault_injection_allocator() = default;

  template <typename U>
  constexpr fault_injection_allocator(const fault_injection_allocator<U>&) noexcept {}

  T* allocate(size_t count) {
    return static_cast<T*>(injected_allocate(count * sizeof(T)));
  }

  void deallocate(void* ptr, size_t) {
    injected_deallocate(ptr);
  }
};

struct fault_injection_context {
  std::vector<size_t, fault_injection_allocator<size_t>> skip_ranges;
  size_t error_index = 0;
  size_t skip_index = 0;
  bool fault_registered = false;
};

thread_local bool disabled = false;
thread_local fault_injection_context* context = nullptr;

thread_local std::size_t new_calls = 0;
thread_local std::size_t delete_calls = 0;

void* injected_allocate(size_t count) {
  if (!disabled) {
    ++new_calls;
  }

  if (should_inject_fault()) {
    throw std::bad_alloc();
  }

  void* ptr = std::malloc(count);
  assert(ptr);
  return ptr;
}

void injected_deallocate(void* ptr) {
  if (!disabled) {
    ++delete_calls;
  }
  std::free(ptr);
}

bool should_inject_fault() {
  if (!context) {
    return false;
  }

  if (disabled) {
    return false;
  }

  assert(context->error_index <= context->skip_ranges.size());
  if (context->error_index == context->skip_ranges.size()) {
    fault_injection_disable dg;
    ++context->error_index;
    context->skip_ranges.push_back(0);
    context->fault_registered = true;
    return true;
  }

  assert(context->skip_index <= context->skip_ranges[context->error_index]);

  if (context->skip_index == context->skip_ranges[context->error_index]) {
    ++context->error_index;
    context->skip_index = 0;
    context->fault_registered = true;
    return true;
  }

  ++context->skip_index;
  return false;
}

void fault_injection_point() {
  if (should_inject_fault()) {
    fault_injection_disable dg;
    throw injected_fault("injected fault");
  }
}

void faulty_run(const std::function<void()>& f) {
  assert(!context);
  fault_injection_context ctx;
  context = &ctx;
  for (;;) {
    try {
      f();
    } catch (...) {
      fault_injection_disable dg;
      ctx.skip_ranges.resize(ctx.error_index);
      ++ctx.skip_ranges.back();
      ctx.error_index = 0;
      ctx.skip_index = 0;
      assert(ctx.fault_registered);
      ctx.fault_registered = false;
      continue;
    }
    assert(!ctx.fault_registered);
    break;
  }
  context = nullptr;
}

fault_injection_disable::fault_injection_disable() : was_disabled(disabled) {
  disabled = true;
}

fault_injection_disable::~fault_injection_disable() {
  disabled = was_disabled;
}

} // namespace

void* operator new(std::size_t count) {
  return injected_allocate(count);
}

void* operator new[](std::size_t count) {
  return injected_allocate(count);
}

void* operator new(std::size_t count, const std::nothrow_t&) noexcept {
  fault_injection_disable dg;
  return injected_allocate(count);
}

void* operator new[](std::size_t count, const std::nothrow_t&) noexcept {
  fault_injection_disable dg;
  return injected_allocate(count);
}

void operator delete(void* ptr) noexcept {
  injected_deallocate(ptr);
}

void operator delete[](void* ptr) noexcept {
  injected_deallocate(ptr);
}

void operator delete(void* ptr, std::size_t) noexcept {
  injected_deallocate(ptr);
}

void operator delete[](void* ptr, std::size_t) noexcept {
  injected_deallocate(ptr);
}

TEST(allocation_calls_test, weak_ptr_allocations) {
  std::size_t new_calls_before = new_calls;
  std::size_t delete_calls_before = delete_calls;
  int* i_p = new int(1337);
  weak_ptr<int> w_p;
  {
    shared_ptr<int> s_p(i_p);
    w_p = s_p;
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 2);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 1);
  EXPECT_FALSE(w_p.lock()); 
}

TEST(allocation_calls_test, make_shared_weak_ptr_allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  weak_ptr<int> w_p;
  {
    shared_ptr<int> s_p = make_shared<int>(42);
    w_p = s_p;
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 1);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 0);
  EXPECT_FALSE(w_p.lock());
}

TEST(allocation_calls_test, allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  int* i_p = new int(1337);
  {
    shared_ptr<int> p(i_p);
    EXPECT_EQ(*i_p, *p);
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 2);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 2);
}

TEST(allocation_calls_test, make_shared_allocations) {
  size_t new_calls_before = new_calls;
  size_t delete_calls_before = delete_calls;
  {
    shared_ptr<int> p = make_shared<int>(42);
    EXPECT_EQ(42, *p);
  }
  const auto new_calls_after = new_calls;
  const auto delete_calls_after = delete_calls;
  EXPECT_EQ(new_calls_after - new_calls_before, 1);
  EXPECT_EQ(delete_calls_after - delete_calls_before, 1);
}

TEST(fault_injection_test, pointer_ctor) {
  faulty_run([] {
    bool deleted = false;
    destruction_tracker* ptr = new destruction_tracker(&deleted);
    try {
      shared_ptr<destruction_tracker> sp(ptr);
    } catch (...) {
      fault_injection_disable dg;
      EXPECT_TRUE(deleted);
      throw;
    }
  });
}

TEST(fault_injection_test, pointer_ctor_with_custom_deleter) {
  faulty_run([] {
    bool deleted = false;
    int* ptr = new int(42);
    try {
      shared_ptr<int> sp(ptr, tracking_deleter<int>(&deleted));
    } catch (...) {
      fault_injection_disable dg;
      EXPECT_TRUE(deleted);
      throw;
    }
  });
}

TEST(fault_injection_test, reset_ptr) {
  faulty_run([] {
    bool deleted1 = false;
    bool deleted2 = false;
    ::disabled = true;
    destruction_tracker* ptr1 = new destruction_tracker(&deleted1);
    destruction_tracker* ptr2 = new destruction_tracker(&deleted2);
    shared_ptr<destruction_tracker> sp(ptr1);
    ::disabled = false;
    try {
      sp.reset(ptr2);
    } catch (...) {
      fault_injection_disable dg;
      EXPECT_TRUE(deleted2);
      EXPECT_FALSE(deleted1);
      EXPECT_TRUE(sp.get() == ptr1);
      throw;
    }
  });
}

TEST(fault_injection_test, reset_ptr_with_custom_deleter) {
  faulty_run([] {
    bool deleted1 = false;
    bool deleted2 = false;
    ::disabled = true;
    int* ptr1 = new int(42);
    int* ptr2 = new int(43);
    shared_ptr<int> sp(ptr1, tracking_deleter<int>(&deleted1));
    ::disabled = false;
    try {
      sp.reset(ptr2, tracking_deleter<int>(&deleted2));
    } catch (...) {
      fault_injection_disable dg;
      EXPECT_TRUE(deleted2);
      EXPECT_FALSE(deleted1);
      EXPECT_TRUE(sp.get() == ptr1);
      throw;
    }
  });
}

TEST(fault_injection_test, make_shared) {
  struct test_object {
    explicit test_object(int value) : value(value) {
      fault_injection_point();
    }

    int value;
  };

  faulty_run([] { shared_ptr<test_object> p = make_shared<test_object>(42); });
}

#endif