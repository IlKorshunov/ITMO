#pragma once

#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>

namespace intrusive {

namespace _impl {
struct list_element_base {
  list_element_base* prev;
  list_element_base* next;

  list_element_base() noexcept;

  list_element_base(const list_element_base&) noexcept;

  list_element_base& operator=(const list_element_base& other) noexcept;

  list_element_base(list_element_base&& other) noexcept;

  list_element_base& operator=(list_element_base&& other) noexcept;

  bool is_linked() const noexcept;

  void unlink() noexcept;

  void link_next(list_element_base* other) noexcept;

  ~list_element_base();
};
} // namespace _impl

class default_tag;

template <typename Tag = default_tag>
class list_element : private _impl::list_element_base {
  template <typename ListT, typename ListTag>
  friend class list;
};

template <typename T, typename Tag = default_tag>
class list {
  static_assert(std::is_base_of_v<list_element<Tag>, T>, "T must derive from list_element");

private:
  using node = _impl::list_element_base;
  node sentinel;

  template <class U>
  class basic_list_iterator;

public:
  using iterator = basic_list_iterator<T>;
  using const_iterator = basic_list_iterator<const T>;

private:
  static node& as_node(T& value) noexcept {
    return static_cast<list_element<Tag>&>(value);
  }

public:
  // O(1)
  list() noexcept = default;

  // O(1)
  ~list() = default;

  list(const list&) = delete;
  list& operator=(const list&) = delete;

  // O(1)
  list(list&& other) noexcept = default;

  // O(1)
  list& operator=(list&& other) noexcept = default;

  // O(1)
  bool empty() const noexcept {
    return !sentinel.is_linked();
  }

  // O(n)
  size_t size() const noexcept {
    return std::distance(begin(), end());
  }

  // O(1)
  T& front() noexcept {
    return *begin();
  }

  // O(1)
  const T& front() const noexcept {
    return *begin();
  }

  // O(1)
  T& back() noexcept {
    return *std::prev(end());
  }

  // O(1)
  const T& back() const noexcept {
    return *std::prev(end());
  }

  // O(1)
  void push_front(T& value) noexcept {
    insert(begin(), value);
  }

  // O(1)
  void push_back(T& value) noexcept {
    insert(end(), value);
  }

  // O(1)
  void pop_front() noexcept {
    erase(begin());
  }

  // O(1)
  void pop_back() noexcept {
    erase(std::prev(end()));
  }

  // O(1)
  void clear() noexcept {
    sentinel.unlink();
  }

  // O(1)
  iterator begin() noexcept {
    return iterator(sentinel.next);
  }

  // O(1)
  const_iterator begin() const noexcept {
    return const_iterator(sentinel.next);
  }

  // O(1)
  iterator end() noexcept {
    return iterator(&sentinel);
  }

  // O(1)
  const_iterator end() const noexcept {
    return const_iterator(const_cast<node*>(&sentinel));
  }

  // O(1)
  iterator insert(const_iterator pos, T& value) noexcept {
    if (pos == &as_node(value)) {
      return iterator(pos._node);
    }
    as_node(value).unlink();
    pos._node->prev->link_next(&as_node(value));
    as_node(value).link_next(pos._node);

    return iterator(&as_node(value));
  }

  // O(1)
  iterator erase(const_iterator pos) noexcept {
    iterator it(pos._node->next);
    pos._node->unlink();
    return it;
  }

  // O(1)
  void splice(const_iterator pos, [[maybe_unused]] list& other, const_iterator first, const_iterator last) noexcept {
    if (first == last) {
      return;
    }

    node* cp = pos._node->prev;
    last._node->prev->link_next(pos._node);
    first._node->prev->link_next(last._node);
    cp->link_next(first._node);
  }
};

template <typename T, typename Tag>
template <typename U>
class list<T, Tag>::basic_list_iterator {
  friend list;

public:
  using value_type = T;
  using reference = U&;
  using pointer = U*;
  using difference_type = ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;

private:
  node* _node;

  basic_list_iterator(node* node) noexcept : _node(node) {}

public:
  basic_list_iterator() noexcept = default;

  operator basic_list_iterator<const U>() const noexcept {
    return basic_list_iterator<const U>(_node);
  }

  basic_list_iterator& operator++() noexcept {
    _node = _node->next;
    return *this;
  }

  basic_list_iterator& operator--() noexcept {
    _node = _node->prev;
    return *this;
  }

  basic_list_iterator operator++(int) noexcept {
    basic_list_iterator copy(*this);
    ++*this;
    return copy;
  }

  basic_list_iterator operator--(int) noexcept {
    basic_list_iterator copy(*this);
    --*this;
    return copy;
  }

  reference operator*() const noexcept {
    return *operator->();
  }

  pointer operator->() const noexcept {
    return static_cast<pointer>(static_cast<list_element<Tag>*>(_node));
  }

  friend bool operator==(const basic_list_iterator& left, const basic_list_iterator& right) noexcept = default;
};

} // namespace intrusive
