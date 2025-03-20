#pragma once

#include <cstddef>
#include <iterator>
#include <type_traits>
#include <utility>

namespace intrusive {

namespace _base {
struct list_element_base {
  list_element_base* prev;
  list_element_base* next;

  list_element_base() noexcept;
  ~list_element_base() noexcept;

  list_element_base(const list_element_base& other) noexcept;
  list_element_base& operator=(const list_element_base& other) noexcept;

  list_element_base(list_element_base&& other) noexcept;
  list_element_base& operator=(list_element_base&& other) noexcept;

  bool is_linked() const noexcept;
  void unlink() noexcept;
};
} // namespace _base

class default_tag;

template <typename Tag = default_tag>
class list_element: private _base::list_element_base {
  template <typename ListT, typename ListTag>
  friend class list;
};

template <typename T, typename Tag = default_tag>
class list {
  static_assert(std::is_base_of_v<list_element<Tag>, T>, "T must derive from list_element");

private:
  using node = _base::list_element_base;
  node dummy;


public:
  template <class U>
  class basic_list_iterator;

  using iterator = basic_list_iterator<T>;
  using const_iterator = basic_list_iterator<const T>;

public:
  // O(1)
  list() noexcept = default;

  // O(1)
  ~list() { clear(); }

  // O(1)
  list(list&& other) noexcept = default;

  // O(1)
  list& operator=(list&& other) noexcept = default;

  list(const list&) = delete;
  list& operator=(const list&) = delete;

  // O(1)
  bool empty() const noexcept {
    return !dummy.is_linked();
  }

  // O(n)
  size_t size() const noexcept {
    size_t out = 0;
    for (const_iterator it = begin(); it != end(); ++it) {
        ++out;
    }
    return out;
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
    insert(const_iterator(dummy.next), value);
  }

  // O(1)
  void push_back(T& value) noexcept {
    insert(const_iterator(&dummy), value);
  }

  // O(1)
  void pop_front() noexcept {
    erase(const_iterator(dummy.next));
  }

  // O(1)
  void pop_back() noexcept {
    erase(const_iterator(dummy.prev));
  }

  // O(1)
  void clear() noexcept {
    dummy.unlink();
  }

  // O(1)
  iterator begin() noexcept {
    return iterator(dummy.next);
  }

  // O(1)
  const_iterator begin() const noexcept {
    return const_iterator(dummy.next);
  }

  // O(1)
  iterator end() noexcept {
    return iterator(&dummy);
  }

  // O(1)
  const_iterator end() const noexcept {
    return const_iterator(const_cast<node*>(&dummy));
  }

  // O(1)
  iterator insert(const_iterator pos, T& value) noexcept {
    node* cur_node = pos._node;
    node* new_node = &static_cast<list_element<Tag>&>(value);
    if (cur_node == new_node) {
      return iterator(new_node);
    }

    if (new_node->is_linked()) {
      new_node->unlink();
    }

    new_node->next = cur_node;
    new_node->prev = cur_node->prev;

    cur_node->prev->next = new_node;
    cur_node->prev = new_node;
    
    return iterator(new_node);
  }

  // O(1)
  iterator erase(const_iterator pos) noexcept {
    node* cur_node = pos._node;
    node* next = cur_node->next;
    cur_node->unlink();
    return iterator(next);
  }

  // O(1)
  void splice(const_iterator pos, [[maybe_unused]] list& other, const_iterator first, const_iterator last) noexcept {
    if (first == last) {
      return;
    }
    node* cur_node = pos._node;
    node* first_node = first._node; 
    node* last_node = last._node->prev;

    // отвязываем в other
    node* before_first = first_node->prev; 
    node* after_last = last._node;

    before_first->next = after_last;
    after_last->prev = before_first;

    // привазываем в текущем списке
    cur_node->prev->next = first_node;
    first_node->prev = cur_node->prev;


    last_node->next = cur_node;
    cur_node->prev = last_node;
  }
};

template <typename T, typename Tag>
template <typename U>
class list<T, Tag>::basic_list_iterator {
  friend list;

private:
  node* _node;

public:
  using value_type = T;
  using reference = U&;
  using pointer = U*;
  using difference_type = std::ptrdiff_t;
  using iterator_category = std::bidirectional_iterator_tag;


  operator basic_list_iterator<const U>() const noexcept {
    return basic_list_iterator<const U>(_node);
  }

  basic_list_iterator() noexcept: _node(nullptr) {}
  explicit basic_list_iterator(node* node) noexcept : _node(node) {}
  basic_list_iterator(std::nullptr_t) = delete;

  reference operator*() const noexcept { return *operator->(); }
  pointer operator->() const noexcept { return static_cast<pointer>(static_cast<list_element<Tag>*>(_node)); }

  basic_list_iterator& operator++() noexcept {
    _node = _node->next;
    return *this;
  }

  basic_list_iterator& operator--() noexcept {
    _node = _node->prev;
    return *this;
  }

  basic_list_iterator operator++(int) noexcept {
    basic_list_iterator  copy(*this);
    _node = _node->next;
    return copy;
  }

  basic_list_iterator operator--(int) noexcept {
    basic_list_iterator  copy(*this);
    _node = _node->prev;
    return copy;
  }

  bool operator==(const basic_list_iterator& other) const noexcept {
    return _node == other._node;
  }

  bool operator!=(const basic_list_iterator& other) const noexcept {
    return _node != other._node;
  }
};

} // namespace intrusive