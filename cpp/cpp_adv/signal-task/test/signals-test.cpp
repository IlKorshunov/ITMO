#include "signals.h"

#include <gtest/gtest.h>

TEST(signal_test, trivial) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_test, arguments) {
  signals::signal<void(int, int, int)> sig;
  [[maybe_unused]] auto conn = sig.connect([](int a, int b, int c) {
    EXPECT_EQ(5, a);
    EXPECT_EQ(6, b);
    EXPECT_EQ(7, c);
  });

  sig(5, 6, 7);
}

TEST(signal_test, arguments_forwarding) {
  struct non_copyable {
    explicit non_copyable(int value) : value(value) {}

    non_copyable(non_copyable&) = delete;
    non_copyable(non_copyable&&) = delete;

    int value;
  };

  using nc = non_copyable;

  signals::signal<void(nc&, nc&&, const nc&, const nc&&)> sig;
  [[maybe_unused]] auto conn = sig.connect([](nc& a, nc&& b, const nc& c, const nc&& d) {
    EXPECT_EQ(5, a.value);
    EXPECT_EQ(6, b.value);
    EXPECT_EQ(7, c.value);
    EXPECT_EQ(8, d.value);
  });

  nc a(5);
  sig(a, nc(6), nc(7), nc(8));
}

TEST(signal_test, empty_connection_move) {
  signals::signal<void()>::connection a;
  signals::signal<void()>::connection b = std::move(a);
  b = std::move(b);
}

TEST(signal_test, disconnect) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  conn1.disconnect();
  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_test, function_destroyed_after_disconnect) {
  bool destroyed = false;

  struct conn1_guard {
    explicit conn1_guard(bool& d) : d(d) {}

    ~conn1_guard() {
      d = true;
    }

    bool& d;
  };

  signals::signal<void()> sig;
  auto conn1 = sig.connect([&, g = conn1_guard(destroyed)] {});
  [[maybe_unused]] auto conn2 = sig.connect([&] {});

  conn1.disconnect();
  EXPECT_TRUE(destroyed);
}

TEST(signal_test, connection_move_ctor) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1_old = sig.connect([&] { ++got1; });
  [[maybe_unused]] auto conn1_new = std::move(conn1_old);

  sig();

  EXPECT_EQ(1, got1);
}

TEST(signal_test, connection_destructor) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = std::make_unique<signals::signal<void()>::connection>(sig.connect([&] { ++got1; }));
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig.connect([&] { ++got2; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);

  conn1.reset();
  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(2, got2);
}

TEST(signal_test, disconnect_inside_emit) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  connection conn2 = sig.connect([&] {
    ++got2;
    conn2.disconnect();
  });
  uint32_t got3 = 0;
  [[maybe_unused]] auto conn3 = sig.connect([&] { ++got3; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(1, got3);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(2, got3);
}

TEST(signal_test, disconnect_other_connection_inside_emit) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  connection conn1 = sig.connect([&] { ++got1; });
  connection conn3;
  connection conn4;
  uint32_t got2 = 0;
  [[maybe_unused]] connection conn2 = sig.connect([&] {
    ++got2;
    conn1.disconnect();
    conn3.disconnect();
    conn4.disconnect();
  });
  uint32_t got3 = 0;
  conn3 = sig.connect([&] { ++got3; });
  uint32_t got4 = 0;
  conn4 = sig.connect([&] { ++got4; });

  sig();

  EXPECT_EQ(1, got2);

  sig();

  EXPECT_LE(0, got1);
  EXPECT_EQ(2, got2);
  EXPECT_LE(0, got3);
  EXPECT_LE(0, got4);
}

TEST(signal_test, connection_destructor_inside_emit) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  std::unique_ptr<connection> conn2 = std::make_unique<connection>(sig.connect([&] {
    ++got2;
    conn2.reset();
  }));
  uint32_t got3 = 0;
  [[maybe_unused]] auto conn3 = sig.connect([&] { ++got3; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(1, got3);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(2, got3);
}

TEST(signal_test, another_connection_destructor_inside_emit) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  std::unique_ptr<connection> conn3;
  [[maybe_unused]] auto cnn2 = sig.connect([&] {
    ++got2;
    conn3.reset();
  });
  uint32_t got3 = 0;
  conn3 = std::make_unique<connection>(sig.connect([&] { ++got3; }));
  uint32_t got4 = 0;
  [[maybe_unused]] auto conn4 = sig.connect([&] { ++got4; });

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(0, got3);
  EXPECT_EQ(1, got4);

  sig();

  EXPECT_EQ(2, got1);
  EXPECT_EQ(2, got2);
  EXPECT_EQ(0, got3);
  EXPECT_EQ(2, got4);
}

TEST(signal_test, disconnect_before_emit) {
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  auto conn1 = sig.connect([&] { ++got1; });

  conn1.disconnect();
  sig();
  EXPECT_EQ(got1, 0);
}

TEST(signal_test, destroy_signal_before_connection_01) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig->connect([&] { ++got1; });

  sig.reset();
}

TEST(signal_test, destroy_signal_before_connection_02) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  auto conn1_old = sig->connect([&] { ++got1; });

  sig.reset();

  [[maybe_unused]] auto conn1_new = std::move(conn1_old);
}

TEST(signal_test, destroy_signal_inside_emit) {
  using connection = signals::signal<void()>::connection;

  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  [[maybe_unused]] connection conn1(sig->connect([&] { ++got1; }));
  uint32_t got2 = 0;
  [[maybe_unused]] connection conn2(sig->connect([&] {
    ++got2;
    sig.reset();
  }));
  uint32_t got3 = 0;
  [[maybe_unused]] connection conn3(sig->connect([&] { ++got3; }));

  (*sig)();

  EXPECT_EQ(1, got2);
}

TEST(signal_test, recursive_emit) {
  auto sig = std::make_unique<signals::signal<void()>>();
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig->connect([&] { ++got1; });
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig->connect([&] {
    ++got2;
    if (got2 == 1) {
      (*sig)();
    } else if (got2 == 2) {
      sig.reset();
    } else {
      FAIL() << "This branch should never execute";
    }
  });
  uint32_t got3 = 0;
  [[maybe_unused]] auto conn3 = sig->connect([&] { ++got3; });

  (*sig)();

  EXPECT_EQ(2, got2);
}

TEST(signal_test, mutual_recursion) {
  using connection = signals::signal<void()>::connection;
  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  connection conn3;
  [[maybe_unused]] auto conn2 = sig.connect([&] {
    ++got2;
    if (got2 == 1) {
      sig();
    } else if (got2 == 2) {
      // do nothing
    } else if (got2 == 3) {
      conn3.disconnect();
    } else {
      FAIL() << "This branch should never execute";
    }
  });
  uint32_t got3 = 0;
  conn3 = sig.connect([&] {
    ++got3;
    if (got3 == 1 && got2 == 2) {
      sig();
    } else {
      FAIL() << "This branch should never execute";
    }
  });
  uint32_t got4 = 0;
  [[maybe_unused]] auto conn4 = sig.connect([&] { ++got4; });

  sig();

  EXPECT_EQ(3, got1);
  EXPECT_EQ(3, got2);
  EXPECT_EQ(1, got3);
  EXPECT_EQ(3, got4);
}

TEST(signal_test, exception_inside_emit) {
  struct test_exception : std::exception {};

  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig.connect([&] {
    ++got2;
    if (got2 == 1) {
      throw test_exception();
    }
  });
  uint32_t got3 = 0;
  [[maybe_unused]] auto conn3 = sig.connect([&] { ++got3; });

  EXPECT_THROW(sig(), test_exception);
  EXPECT_EQ(1, got2);

  got1 = 0;
  got3 = 0;

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(2, got2);
  EXPECT_EQ(1, got3);
}

TEST(signal_test, exception_inside_recursive_emit) {
  struct test_exception : std::exception {};

  signals::signal<void()> sig;
  uint32_t got1 = 0;
  [[maybe_unused]] auto conn1 = sig.connect([&] { ++got1; });
  uint32_t got2 = 0;
  [[maybe_unused]] auto conn2 = sig.connect([&] {
    ++got2;
    if (got2 == 1) {
      sig();
    } else if (got2 == 2) {
      throw test_exception();
    }
  });
  uint32_t got3 = 0;
  [[maybe_unused]] auto conn3 = sig.connect([&] { ++got3; });

  EXPECT_THROW(sig(), test_exception);
  EXPECT_EQ(2, got2);

  got1 = 0;
  got3 = 0;

  sig();

  EXPECT_EQ(1, got1);
  EXPECT_EQ(3, got2);
  EXPECT_EQ(1, got3);
}

TEST(signal_test, move_connection_inside_emit) {
  using connection = signals::signal<void()>::connection;

  signals::signal<void()> sig;
  uint32_t got1 = 0;
  std::unique_ptr<connection> conn1_new;

  connection conn1_old = sig.connect([&] {
    ++got1;
    if (got1 == 1) {
      conn1_new = std::make_unique<connection>(std::move(conn1_old));
    }
  });

  sig();
  EXPECT_EQ(1, got1);

  sig();
  EXPECT_EQ(2, got1);
}

TEST(signal_test, move_other_connection_inside_emit) {
  using connection = signals::signal<void()>::connection;

  signals::signal<void()> sig;

  uint32_t got1 = 0;
  std::unique_ptr<connection> conn1_old;
  std::unique_ptr<connection> conn1_new;

  uint32_t got3 = 0;
  std::unique_ptr<connection> conn3_old;
  std::unique_ptr<connection> conn3_new;

  conn1_old = std::make_unique<connection>(sig.connect([&] { ++got1; }));

  uint32_t got2 = 0;
  [[maybe_unused]] connection conn2 = sig.connect([&] {
    ++got2;
    conn1_new = std::make_unique<connection>(std::move(*conn1_old));
    conn3_new = std::make_unique<connection>(std::move(*conn3_old));
    conn1_old.reset();
    conn3_old.reset();
  });

  conn3_old = std::make_unique<connection>(sig.connect([&] { ++got3; }));

  sig();
  EXPECT_EQ(1, got1);
  EXPECT_EQ(1, got2);
  EXPECT_EQ(1, got3);
}
