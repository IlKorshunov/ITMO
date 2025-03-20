#include "matrix.h"
#include "test-helpers.h"

#include <gtest/gtest.h>

#include <numeric>

TEST(operations_test, element_access) {
  matrix<int> a(2, 3);

  a(0, 2) = 5;
  a(1, 1) = 42;
  a(1, 2) = a(0, 2);

  const matrix<int>& ca = a;

  EXPECT_EQ(0, ca(0, 0));
  EXPECT_EQ(0, ca(0, 1));
  EXPECT_EQ(5, ca(0, 2));
  EXPECT_EQ(0, ca(1, 0));
  EXPECT_EQ(42, ca(1, 1));
  EXPECT_EQ(5, ca(1, 2));
}

TEST(operations_test, data) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;

  matrix<int> a(ROWS, COLS);
  fill(a);

  {
    int* data = a.data();
    for (size_t i = 0; i < ROWS; ++i) {
      for (size_t j = 0; j < COLS; ++j) {
        EXPECT_EQ(elem(i, j), data[i * COLS + j]);
      }
    }
  }

  {
    const int* data = std::as_const(a).data();
    for (size_t i = 0; i < ROWS; ++i) {
      for (size_t j = 0; j < COLS; ++j) {
        EXPECT_EQ(elem(i, j), data[i * COLS + j]);
      }
    }
  }
}

TEST(operations_test, range_based_for) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;

  matrix<int> a(ROWS, COLS);
  fill(a);

  for (size_t i = 0; int x : a) {
    EXPECT_EQ(a(i / COLS, i % COLS), x);
    ++i;
  }

  for (size_t i = 0; int x : std::as_const(a)) {
    EXPECT_EQ(a(i / COLS, i % COLS), x);
    ++i;
  }

  for (size_t i = 0; const int& x : a) {
    EXPECT_EQ(a.data() + i, &x);
    EXPECT_EQ(a(i / COLS, i % COLS), x);
    ++i;
  }

  for (size_t i = 0; const int& x : std::as_const(a)) {
    EXPECT_EQ(a.data() + i, &x);
    EXPECT_EQ(a(i / COLS, i % COLS), x);
    ++i;
  }

  for (size_t i = 0; int& x : a) {
    EXPECT_EQ(a.data() + i, &x);
    EXPECT_EQ(a(i / COLS, i % COLS), x);
    x += 2;
    ++i;
  }

  for (size_t i = 0; int x : a) {
    EXPECT_EQ(elem(i / COLS, i % COLS) + 2, x);
    ++i;
  }
}

namespace {

template <class It>
class view {
public:
  view(It begin, It end) : _begin(begin), _end(end) {}

  It begin() const {
    return _begin;
  }

  It end() const {
    return _end;
  }

private:
  It _begin;
  It _end;
};

} // namespace

TEST(operations_test, row_range_based_for) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;
  constexpr size_t CHOSEN_ROW = 2;

  matrix<int> a(ROWS, COLS);
  fill(a);

  view row_view(a.row_begin(CHOSEN_ROW), a.row_end(CHOSEN_ROW));
  view const_row_view(std::as_const(a).row_begin(CHOSEN_ROW), std::as_const(a).row_end(CHOSEN_ROW));

  for (size_t i = 0; int x : row_view) {
    EXPECT_EQ(a(CHOSEN_ROW, i), x);
    ++i;
  }

  for (size_t i = 0; int x : const_row_view) {
    EXPECT_EQ(a(CHOSEN_ROW, i), x);
    ++i;
  }

  for (size_t i = 0; const int& x : row_view) {
    EXPECT_EQ(a.data() + COLS * CHOSEN_ROW + i, &x);
    EXPECT_EQ(a(CHOSEN_ROW, i), x);
    ++i;
  }

  for (size_t i = 0; const int& x : const_row_view) {
    EXPECT_EQ(a.data() + COLS * CHOSEN_ROW + i, &x);
    EXPECT_EQ(a(CHOSEN_ROW, i), x);
    ++i;
  }

  for (size_t i = 0; int& x : row_view) {
    EXPECT_EQ(a.data() + COLS * CHOSEN_ROW + i, &x);
    EXPECT_EQ(a(CHOSEN_ROW, i), x);
    x += 2;
    ++i;
  }

  for (size_t i = 0; int x : a) {
    size_t row = i / COLS;
    size_t col = i % COLS;
    if (row == CHOSEN_ROW) {
      EXPECT_EQ(elem(row, col) + 2, x);
    } else {
      EXPECT_EQ(elem(row, col), x);
    }
    ++i;
  }
}

TEST(operations_test, col_range_based_for) {
  constexpr size_t ROWS = 40;
  constexpr size_t COLS = 100;
  constexpr size_t CHOSEN_COL = 2;

  matrix<int> a(ROWS, COLS);
  fill(a);

  view col_view(a.col_begin(CHOSEN_COL), a.col_end(CHOSEN_COL));
  view const_col_view(std::as_const(a).col_begin(CHOSEN_COL), std::as_const(a).col_end(CHOSEN_COL));

  for (size_t i = 0; int x : col_view) {
    EXPECT_EQ(a(i, CHOSEN_COL), x);
    ++i;
  }

  for (size_t i = 0; int x : const_col_view) {
    EXPECT_EQ(a(i, CHOSEN_COL), x);
    ++i;
  }

  for (size_t i = 0; const int& x : col_view) {
    EXPECT_EQ(a.data() + CHOSEN_COL + COLS * i, &x);
    EXPECT_EQ(a(i, CHOSEN_COL), x);
    ++i;
  }

  for (size_t i = 0; const int& x : const_col_view) {
    EXPECT_EQ(a.data() + CHOSEN_COL + COLS * i, &x);
    EXPECT_EQ(a(i, CHOSEN_COL), x);
    ++i;
  }

  for (size_t i = 0; int& x : col_view) {
    EXPECT_EQ(a.data() + CHOSEN_COL + COLS * i, &x);
    EXPECT_EQ(a(i, CHOSEN_COL), x);
    x += 2;
    ++i;
  }

  for (size_t i = 0; int x : a) {
    size_t row = i / COLS;
    size_t col = i % COLS;
    if (col == CHOSEN_COL) {
      EXPECT_EQ(elem(row, col) + 2, x);
    } else {
      EXPECT_EQ(elem(row, col), x);
    }
    ++i;
  }
}

TEST(operations_test, compare) {
  matrix<int> a({
      {10, 20, 30},
      {40, 50, 60},
  });
  matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });
  matrix<int> c({
      {10, 20, 30},
      {42, 50, 60},
  });
  matrix<int> d({
      {10, 20},
      {30, 40},
      {50, 60},
  });
  matrix<int> e({
      {10, 20, 30},
  });
  matrix<int> f({
      {10},
      {40},
  });

  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);

  EXPECT_TRUE(a != c);
  EXPECT_FALSE(a == c);

  EXPECT_TRUE(a != d);
  EXPECT_FALSE(a == d);

  EXPECT_TRUE(a != e);
  EXPECT_FALSE(a == e);

  EXPECT_TRUE(a != f);
  EXPECT_FALSE(a == f);
}

TEST(operations_test, compare_empty) {
  matrix<int> a, b;
  EXPECT_TRUE(a == b);
  EXPECT_FALSE(a != b);
}

TEST(operations_test, add) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });
  const matrix<int> c({
      {11, 22, 33},
      {44, 55, 66},
  });

  expect_equal(c, a + b);

  a += b;
  expect_equal(c, a);
}

TEST(operations_test, add_return_value) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });
  const matrix<int> c({
      {21,  42,  63},
      {84, 105, 126},
  });

  (a += b) += b;
  expect_equal(c, a);
}

TEST(operations_test, sub) {
  matrix<int> a({
      {11, 22, 33},
      {44, 55, 66},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });
  const matrix<int> c({
      {1, 2, 3},
      {4, 5, 6},
  });

  expect_equal(c, a - b);

  a -= b;
  expect_equal(c, a);
}

TEST(operations_test, sub_return_value) {
  matrix<int> a({
      {21,  42,  63},
      {84, 105, 126},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });
  const matrix<int> c({
      {1, 2, 3},
      {4, 5, 6},
  });

  (a -= b) -= b;
  expect_equal(c, a);
}

TEST(operations_test, mul) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
      {7, 8, 9},
  });
  const matrix<int> b({
      {10, 40},
      {20, 50},
      {30, 60},
  });
  const matrix<int> c({
      {140,  320},
      {320,  770},
      {500, 1220},
  });

  expect_equal(c, a * b);

  a *= b;
  expect_equal(c, a);
}

TEST(operations_test, mul_return_value) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
      {7, 8, 9},
  });
  const matrix<int> b({
      {10, 40},
      {20, 50},
      {30, 60},
  });
  const matrix<int> c({
      {150,  360},
      {340,  820},
      {530, 1280},
  });

  (a *= b) += b;
  expect_equal(c, a);
}

TEST(operations_test, mul_scalar) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });

  expect_equal(b, a * 10);
  expect_equal(b, 10 * a);

  a *= 10;
  expect_equal(b, a);
}

TEST(operations_test, mul_scalar_return_value) {
  matrix<int> a({
      {1, 2, 3},
      {4, 5, 6},
  });
  const matrix<int> b({
      {10, 20, 30},
      {40, 50, 60},
  });

  (a *= 5) *= 2;
  expect_equal(b, a);
}
