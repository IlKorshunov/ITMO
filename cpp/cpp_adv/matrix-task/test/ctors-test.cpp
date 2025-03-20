#include "matrix.h"
#include "test-helpers.h"

#include <gtest/gtest.h>

namespace {

class element {
public:
  element() : _value(0) {
    ++objects;
    ++default_ctor_calls;
  }

  element(size_t value) : _value(value) {
    ++objects;
    ++value_ctor_calls;
  }

  element& operator=(const element& other) {
    _value = other._value;
    ++copy_assign_calls;
    return *this;
  }

  ~element() {
    --objects;
  }

  friend bool operator==(const element&, const element&) = default;

  friend bool operator==(const element& e, size_t n) {
    return e._value == n;
  }

public:
  inline static size_t objects;
  inline static size_t default_ctor_calls;
  inline static size_t value_ctor_calls;
  inline static size_t copy_assign_calls;

private:
  size_t _value;
};

struct expected_counters {
  size_t default_ctor_calls;
  size_t value_ctor_calls;
  size_t copy_assign_calls;
};

void expect_counters(expected_counters expected_counters) {
  EXPECT_EQ(expected_counters.default_ctor_calls, element::default_ctor_calls);
  EXPECT_EQ(expected_counters.value_ctor_calls, element::value_ctor_calls);
  EXPECT_EQ(expected_counters.copy_assign_calls, element::copy_assign_calls);
}

class ctors_test : public ::testing::Test {
protected:
  void SetUp() override {
    element::objects = 0;
    element::default_ctor_calls = 0;
    element::value_ctor_calls = 0;
    element::copy_assign_calls = 0;
  }

  void TearDown() override {
    EXPECT_EQ(0, element::objects);
  }
};

} // namespace

TEST_F(ctors_test, default_ctor) {
  matrix<element> a;

  expect_empty(a);

  expect_counters({
      .default_ctor_calls = 0,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}

TEST_F(ctors_test, zeros_ctor) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;
  constexpr size_t SIZE = ROWS * COLS;

  matrix<element> a(ROWS, COLS);

  EXPECT_EQ(ROWS, a.rows());
  EXPECT_EQ(COLS, a.cols());
  EXPECT_EQ(SIZE, a.size());
  EXPECT_FALSE(a.empty());
  EXPECT_NE(nullptr, a.data());

  for (size_t i = 0; i < ROWS; ++i) {
    for (size_t j = 0; j < COLS; ++j) {
      EXPECT_EQ(0, a(i, j));
    }
  }

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}

TEST_F(ctors_test, zeros_ctor_empty) {
  matrix<element> a(0, 0);
  expect_empty(a);

  expect_counters({
      .default_ctor_calls = 0,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}

TEST_F(ctors_test, zeros_ctor_weird) {
  matrix<element> a(10, 0);
  expect_empty(a);

  matrix<element> b(0, 10);
  expect_empty(b);

  expect_counters({
      .default_ctor_calls = 0,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}

TEST_F(ctors_test, init_ctor) {
  matrix<element> a({
      {10, 20, 30},
      {40, 50, 60},
  });

  EXPECT_EQ(2, a.rows());
  EXPECT_EQ(3, a.cols());
  EXPECT_EQ(6, a.size());
  EXPECT_FALSE(a.empty());
  EXPECT_NE(nullptr, a.data());

  EXPECT_EQ(10, a(0, 0));
  EXPECT_EQ(20, a(0, 1));
  EXPECT_EQ(30, a(0, 2));
  EXPECT_EQ(40, a(1, 0));
  EXPECT_EQ(50, a(1, 1));
  EXPECT_EQ(60, a(1, 2));

  expect_counters({
      .default_ctor_calls = 6,
      .value_ctor_calls = 6,
      .copy_assign_calls = 6,
  });
}

TEST_F(ctors_test, copy_ctor) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;
  constexpr size_t SIZE = ROWS * COLS;

  matrix<element> a(ROWS, COLS);

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });

  fill(a);

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = SIZE,
      .copy_assign_calls = SIZE,
  });

  matrix<element> b = a;

  expect_equal(a, b);

  expect_counters({
      .default_ctor_calls = SIZE * 2,
      .value_ctor_calls = SIZE,
      .copy_assign_calls = SIZE * 2,
  });
}

TEST_F(ctors_test, copy_ctor_empty) {
  matrix<element> a;
  matrix<element> b = a;

  expect_empty(a);
  expect_empty(b);

  expect_counters({
      .default_ctor_calls = 0,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}

TEST_F(ctors_test, copy_assignment) {
  constexpr size_t ROWS_A = 40;
  constexpr size_t COLS_A = 100;
  constexpr size_t SIZE_A = ROWS_A * COLS_A;

  constexpr size_t ROWS_B = 15;
  constexpr size_t COLS_B = 15;
  constexpr size_t SIZE_B = ROWS_B * COLS_B;

  matrix<element> b(ROWS_B, COLS_B);

  {
    matrix<element> a(ROWS_A, COLS_A);

    expect_counters({
        .default_ctor_calls = SIZE_A + SIZE_B,
        .value_ctor_calls = 0,
        .copy_assign_calls = 0,
    });

    fill(a);

    expect_counters({
        .default_ctor_calls = SIZE_A + SIZE_B,
        .value_ctor_calls = SIZE_A,
        .copy_assign_calls = SIZE_A,
    });

    b = a;

    expect_equal(a, b);

    expect_counters({
        .default_ctor_calls = SIZE_A * 2 + SIZE_B,
        .value_ctor_calls = SIZE_A,
        .copy_assign_calls = SIZE_A * 2,
    });
  }

  for (size_t i = 0; i < ROWS_A; ++i) {
    for (size_t j = 0; j < COLS_A; ++j) {
      EXPECT_EQ(elem(i, j), b(i, j));
    }
  }
}

TEST_F(ctors_test, self_copy_assignment) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;
  constexpr size_t SIZE = ROWS * COLS;

  matrix<element> a(ROWS, COLS);

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });

  fill(a);

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = SIZE,
      .copy_assign_calls = SIZE,
  });

  a = a;

  expect_counters({
      .default_ctor_calls = SIZE,
      .value_ctor_calls = SIZE,
      .copy_assign_calls = SIZE,
  });

  for (size_t i = 0; i < ROWS; ++i) {
    for (size_t j = 0; j < COLS; ++j) {
      EXPECT_EQ(elem(i, j), a(i, j));
    }
  }
}

TEST_F(ctors_test, copy_assignment_empty) {
  constexpr size_t ROWS_B = 40;
  constexpr size_t COLS_B = 100;
  constexpr size_t SIZE_B = ROWS_B * COLS_B;

  matrix<element> a;
  matrix<element> b(ROWS_B, COLS_B);
  b = a;

  expect_empty(a);
  expect_empty(b);

  expect_counters({
      .default_ctor_calls = SIZE_B,
      .value_ctor_calls = 0,
      .copy_assign_calls = 0,
  });
}
