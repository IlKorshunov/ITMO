#include "intrusive-list.h"

namespace intrusive::_impl {

list_element_base::list_element_base() noexcept : prev(this), next(this) {}

list_element_base::list_element_base(const list_element_base&) noexcept : list_element_base() {}

list_element_base& list_element_base::operator=(const list_element_base& other) noexcept {
  if (this != &other) {
    unlink();
  }
  return *this;
}

list_element_base::list_element_base(list_element_base&& other) noexcept : list_element_base() {
  *this = std::move(other);
}

list_element_base& list_element_base::operator=(list_element_base&& other) noexcept {
  if (this == &other) {
    return *this;
  }

  unlink();

  if (other.is_linked()) {
    prev = std::exchange(other.prev, &other);
    next = std::exchange(other.next, &other);
    prev->next = this;
    next->prev = this;
  }

  return *this;
}

bool list_element_base::is_linked() const noexcept {
  return prev != this;
}

void list_element_base::unlink() noexcept {
  prev->next = next;
  next->prev = prev;
  prev = next = this;
}

void list_element_base::link_next(list_element_base* other) noexcept {
  next = other;
  other->prev = this;
}

list_element_base::~list_element_base() {
  unlink();
}

} // namespace intrusive::_impl
