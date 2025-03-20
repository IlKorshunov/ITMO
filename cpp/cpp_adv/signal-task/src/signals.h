#pragma once
#include "intrusive-list.h"

#include <functional>

namespace signals {

namespace detail {
struct signal_tag;
} // namespace detail

template <typename T>
class signal;

template <typename... Args>
class signal<void(Args...)> {
public:
  class connection;

private:
  using slot_t = std::function<void(Args...)>;
  using tag_t = signals::detail::signal_tag;
  using list_t = intrusive::list<connection, tag_t>;
  using list_iterator_t = typename intrusive::list<connection, tag_t>::iterator;
  using connection_base_t = intrusive::list_element<tag_t>;

  struct iterator_token_t {
    list_iterator_t it;
    iterator_token_t* prev;
    iterator_token_t*& tail;

    iterator_token_t(list_iterator_t it, iterator_token_t*& tokens_tail)
        : it(it)
        , prev(tokens_tail)
        , tail(tokens_tail) {
      tokens_tail = this;
    }

    ~iterator_token_t() {
      if (operator bool()) {
        tail = prev;
      }
    }

    operator bool() {
      return it != list_iterator_t{};
    }
  };

public:
  class connection : public connection_base_t {
    friend signal;

  public:
    connection() = default;

    connection(const connection&) = delete;
    connection& operator=(const connection&) = delete;

    connection(connection&& other) noexcept {
      *this = std::move(other);
    }

    connection& operator=(connection&& other) noexcept {
      if (this == &other) {
        return *this;
      }

      disconnect();

      if (other.sig == nullptr) {
        return *this;
      }

      slot = std::move(other.slot);
      sig = other.sig;
      sig->_slots.insert(std::next(list_t::get_iterator(other)), *this);
      other.disconnect();

      return *this;
    }

    void disconnect() noexcept {
      if (sig == nullptr) {
        return;
      }

      for (iterator_token_t* t = sig->_tokens_tail; t != nullptr; t = t->prev) {
        if (t->it != sig->_slots.end() && this == std::to_address(t->it)) {
          ++t->it;
        }
      }

      sig = nullptr;
      unlink();
      slot = {};
    }

    ~connection() {
      disconnect();
    }

  private:
    connection(slot_t&& slot, signal* sig)
        : slot(std::move(slot))
        , sig(sig) {}

    slot_t slot;
    signal* sig = nullptr;
  };

  signal() = default;

  signal(const signal&) = delete;
  signal& operator=(const signal&) = delete;

  ~signal() {
    clear_tokens();

    for (auto it = _slots.begin(); it != _slots.end();) {
      auto old = it;
      ++it;
      old->disconnect();
    }
  }

  connection connect(slot_t slot) {
    connection c{std::move(slot), this};
    _slots.push_back(c);
    return c;
  }

  void operator()(Args... args) const {
    iterator_token_t token(_slots.begin(), _tokens_tail);

    while (token && token.it != _slots.end()) {
      auto old = token.it;
      ++token.it;
      old->slot(std::forward<Args>(args)...);
    }
  }

private:
  void clear_tokens() const noexcept {
    for (iterator_token_t* t = _tokens_tail; t != nullptr; t = t->prev) {
      t->it = {};
    }
    _tokens_tail = nullptr;
  }

  mutable iterator_token_t* _tokens_tail = nullptr;
  mutable list_t _slots;
};

} // namespace signals
