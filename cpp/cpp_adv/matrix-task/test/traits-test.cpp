#include "matrix.h"

#include <gtest/gtest.h>

namespace {

struct element {};

} // namespace

TEST(traits_test, member_types) {
  EXPECT_TRUE((std::is_same_v<element, matrix<element>::value_type>));

  EXPECT_TRUE((std::is_same_v<element&, matrix<element>::reference>));
  EXPECT_TRUE((std::is_same_v<const element&, matrix<element>::const_reference>));

  EXPECT_TRUE((std::is_same_v<element*, matrix<element>::pointer>));
  EXPECT_TRUE((std::is_same_v<const element*, matrix<element>::const_pointer>));

  EXPECT_TRUE((std::is_same_v<element*, matrix<element>::iterator>));
  EXPECT_TRUE((std::is_same_v<const element*, matrix<element>::const_iterator>));

  EXPECT_TRUE((std::is_same_v<element*, matrix<element>::row_iterator>));
  EXPECT_TRUE((std::is_same_v<const element*, matrix<element>::const_row_iterator>));
}

TEST(traits_test, iterator_categories) {
  EXPECT_TRUE(std::contiguous_iterator<matrix<element>::iterator>);
  EXPECT_TRUE(std::contiguous_iterator<matrix<element>::const_iterator>);

  EXPECT_TRUE(std::contiguous_iterator<matrix<element>::row_iterator>);
  EXPECT_TRUE(std::contiguous_iterator<matrix<element>::const_row_iterator>);

  EXPECT_TRUE(std::random_access_iterator<matrix<element>::col_iterator>);
  EXPECT_TRUE(std::random_access_iterator<matrix<element>::const_col_iterator>);
}

TEST(traits_test, range_categories) {
  EXPECT_TRUE(std::ranges::contiguous_range<matrix<element>>);
  EXPECT_TRUE(std::ranges::contiguous_range<const matrix<element>>);
}

TEST(traits_test, iterator_member_types) {
  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<element&, std::iterator_traits<matrix<element>::iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<element*, std::iterator_traits<matrix<element>::iterator>::pointer>));

  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::const_iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<const element&, std::iterator_traits<matrix<element>::const_iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<const element*, std::iterator_traits<matrix<element>::const_iterator>::pointer>));
}

TEST(traits_test, row_iterator_member_types) {
  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::row_iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<element&, std::iterator_traits<matrix<element>::row_iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<element*, std::iterator_traits<matrix<element>::row_iterator>::pointer>));

  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::const_row_iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<const element&, std::iterator_traits<matrix<element>::const_row_iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<const element*, std::iterator_traits<matrix<element>::const_row_iterator>::pointer>));
}

TEST(traits_test, col_iterator_member_types) {
  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::col_iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<element&, std::iterator_traits<matrix<element>::col_iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<element*, std::iterator_traits<matrix<element>::col_iterator>::pointer>));

  EXPECT_TRUE((std::is_same_v<element, std::iterator_traits<matrix<element>::const_col_iterator>::value_type>));
  EXPECT_TRUE((std::is_same_v<const element&, std::iterator_traits<matrix<element>::const_col_iterator>::reference>));
  EXPECT_TRUE((std::is_same_v<const element*, std::iterator_traits<matrix<element>::const_col_iterator>::pointer>));
}

TEST(traits_test, col_iterator_triviality) {
  EXPECT_TRUE(std::is_trivial_v<matrix<element>::col_iterator>);
  EXPECT_TRUE(std::is_trivial_v<matrix<element>::const_col_iterator>);
}
