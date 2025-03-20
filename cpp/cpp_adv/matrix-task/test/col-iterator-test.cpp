#include "matrix.h"

#include <gtest/gtest.h>

namespace {

struct element {
  element() = default;

  element(int value) : value(value) {}

  friend bool operator==(element, element) = default;

  int value;
};

class col_iterator : public ::testing::Test {
protected:
  using matrix = ::matrix<element>;

  matrix m = matrix({
      { 1,  2,  3},
      { 4,  5,  6},
      {11, 12, 13},
      {14, 15, 16},
  });
};

bool operator==(matrix<element>::const_iterator it, matrix<element>::const_col_iterator col_it) {
  return it == col_it.operator->();
}

template <std::random_access_iterator It>
It advance(It it, std::iter_difference_t<It> n) {
  while (n > 0) {
    --n;
    ++it;
  }
  while (n < 0) {
    ++n;
    --it;
  }
  return it;
}

std::ptrdiff_t as_diff(std::size_t n) {
  return static_cast<std::ptrdiff_t>(n);
}

} // namespace

TEST_F(col_iterator, non_const_to_const) {
  matrix::col_iterator begin = m.col_begin(1);
  matrix::const_col_iterator cbegin = begin;
  EXPECT_EQ(std::as_const(m).col_begin(1), cbegin);
  EXPECT_EQ(begin, cbegin);

  matrix::col_iterator end = m.col_end(1);
  matrix::const_col_iterator cend = end;
  EXPECT_EQ(std::as_const(m).col_end(1), cend);
  EXPECT_EQ(end, cend);
}

TEST_F(col_iterator, copy_ctor) {
  matrix::col_iterator it1 = m.col_begin(1);
  matrix::col_iterator it2 = it1;
  EXPECT_EQ(it1, it2);

  ++it2;
  EXPECT_NE(it1, it2);
}

TEST_F(col_iterator, default_construct) {
  matrix::col_iterator it;
  it = m.col_begin(1);
  matrix::const_col_iterator cit;
  cit = m.col_begin(1);
  EXPECT_EQ(it, cit);
}

TEST_F(col_iterator, indirection) {
  auto it = m.col_begin(1);
  auto end = std::as_const(m).col_end(1);

  for (size_t i = 0; i < m.rows(); ++i) {
    ASSERT_NE(it, end);
    EXPECT_EQ(m(i, 1), *it);
    *it = 42;
    ++it;
  }
  EXPECT_EQ(it, end);

  std::for_each(m.col_begin(1), m.col_end(1), [](element x) { EXPECT_EQ(42, x); });
}

TEST_F(col_iterator, member_access) {
  auto it = m.col_begin(1);
  auto end = std::as_const(m).col_end(1);

  for (size_t i = 0; i < m.rows(); ++i) {
    ASSERT_NE(it, end);
    EXPECT_EQ(m(i, 1).value, it->value);
    it->value = 42;
    ++it;
  }
  EXPECT_EQ(it, end);

  std::for_each(m.col_begin(1), m.col_end(1), [](element x) { EXPECT_EQ(42, x); });
}

TEST_F(col_iterator, increment) {
  auto it = std::as_const(m).col_begin(1);

  EXPECT_EQ(&m(1, 1), ++it);
  EXPECT_EQ(&m(1, 1), it);

  EXPECT_EQ(&m(1, 1), it++);
  EXPECT_EQ(&m(2, 1), it);
}

TEST_F(col_iterator, decrement) {
  auto it = std::as_const(m).col_end(1);

  EXPECT_EQ(&m(3, 1), --it);
  EXPECT_EQ(&m(3, 1), it);

  EXPECT_EQ(&m(3, 1), it--);
  EXPECT_EQ(&m(2, 1), it);
}

TEST_F(col_iterator, add_diff) {
  auto it = std::as_const(m).col_begin(1);

  EXPECT_EQ(m.col_end(1), it + m.rows());

  for (std::ptrdiff_t i = 0; i <= m.rows(); ++i) {
    EXPECT_EQ(advance(it, i), it + i);
    EXPECT_EQ(advance(it, i), i + it);

    auto it2 = it;
    it2 += i;
    EXPECT_EQ(advance(it, i), it2);
  }
}

TEST_F(col_iterator, sub_diff) {
  auto it = std::as_const(m).col_end(1);

  EXPECT_EQ(m.col_begin(1), it - m.rows());

  for (std::ptrdiff_t i = 0; i <= m.rows(); ++i) {
    EXPECT_EQ(advance(it, -i), it - i);

    auto it2 = it;
    it2 -= i;
    EXPECT_EQ(advance(it, -i), it2);
  }
}

TEST_F(col_iterator, add_neg_diff) {
  auto it = std::as_const(m).col_end(1);

  EXPECT_EQ(m.col_begin(1), it + -as_diff(m.rows()));

  for (std::ptrdiff_t i = 0; i <= m.rows(); ++i) {
    EXPECT_EQ(advance(it, -i), it + -i);
    EXPECT_EQ(advance(it, -i), -i + it);

    auto it2 = it;
    it2 += -i;
    EXPECT_EQ(advance(it, -i), it2);
  }
}

TEST_F(col_iterator, sub_neg_diff) {
  auto it = std::as_const(m).col_begin(1);

  EXPECT_EQ(m.col_end(1), it - -as_diff(m.rows()));

  for (std::ptrdiff_t i = 0; i <= m.rows(); ++i) {
    EXPECT_EQ(advance(it, i), it - -i);

    auto it2 = it;
    it2 -= -i;
    EXPECT_EQ(advance(it, i), it2);
  }
}

TEST_F(col_iterator, iter_diff) {
  auto it = std::as_const(m).col_begin(1);
  auto end = m.col_end(1);

  EXPECT_EQ(0, it - it);
  EXPECT_EQ(0, end - end);
  EXPECT_EQ(m.rows(), end - it);
  EXPECT_EQ(-as_diff(m.rows()), it - end);

  ++it;
  --end;

  EXPECT_EQ(m.rows() - 2, end - it);
  EXPECT_EQ(-as_diff(m.rows() - 2), it - end);
}

TEST_F(col_iterator, subscript) {
  auto it = std::as_const(m).col_begin(1);

  EXPECT_EQ(m(0, 1), it[0]);
  EXPECT_EQ(m(1, 1), it[1]);

  EXPECT_EQ(&m(0, 1), &it[0]);
  EXPECT_EQ(&m(1, 1), &it[1]);

  ++it;

  EXPECT_EQ(m(0, 1), it[-1]);
  EXPECT_EQ(m(1, 1), it[0]);
  EXPECT_EQ(m(2, 1), it[1]);

  EXPECT_EQ(&m(0, 1), &it[-1]);
  EXPECT_EQ(&m(1, 1), &it[0]);
  EXPECT_EQ(&m(2, 1), &it[1]);
}

TEST_F(col_iterator, compare) {
  auto it1 = std::as_const(m).col_begin(1);
  auto it2 = it1;

  EXPECT_EQ(it1, it2);
  EXPECT_LE(it1, it2);
  EXPECT_GE(it1, it2);

  EXPECT_FALSE(it1 < it2);
  EXPECT_FALSE(it1 > it2);

  ++it2;
  EXPECT_NE(it1, it2);
  EXPECT_LE(it1, it2);
  EXPECT_LT(it1, it2);
  EXPECT_GE(it2, it1);
  EXPECT_GT(it2, it1);

  auto end = m.col_end(1);

  EXPECT_EQ(end, end);
  EXPECT_LE(end, end);
  EXPECT_GE(end, end);

  EXPECT_FALSE(end < end);
  EXPECT_FALSE(end > end);

  EXPECT_NE(it2, end);
  EXPECT_LT(it2, end);
  EXPECT_GT(end, it2);
}
