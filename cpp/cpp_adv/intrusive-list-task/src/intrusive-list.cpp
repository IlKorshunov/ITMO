#include "intrusive_list.h"

namespace intrusive::_base {
list_element_base::list_element_base() noexcept
    : prev(this), next(this)
{ }

list_element_base::~list_element_base() noexcept {
  unlink();
}

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
  
  if (!other.is_linked()) {
    return *this;
  }

  prev = other.prev;
  next = other.next;

  prev->next = this;
  next->prev = this;

  other.prev = &other;
  other.next = &other;

  return *this;
}

bool list_element_base::is_linked() const noexcept {
  return prev != this && next != this;
}

void list_element_base::unlink() noexcept {
  prev->next = next;
  next->prev = prev;
  prev = next = this;
}

} // namespace intrusive::_base
