#include "intrusive_list.h"
#include "test-utils.h"

#include <concepts>
#include <iterator>

TEST(iterator_test, categories) {
  EXPECT_TRUE(std::bidirectional_iterator<intrusive::list<node>::iterator>);
  EXPECT_TRUE(std::bidirectional_iterator<intrusive::list<node>::const_iterator>);
}

TEST(iterator_test, public_ctors) {
  EXPECT_FALSE((std::constructible_from<intrusive::list<node>::iterator, node*>));
  EXPECT_FALSE((std::constructible_from<intrusive::list<node>::const_iterator, const node*>));

  EXPECT_FALSE((std::constructible_from<intrusive::list<node>::iterator, std::nullptr_t>));
  EXPECT_FALSE((std::constructible_from<intrusive::list<node>::const_iterator, std::nullptr_t>));
}

TEST(iterator_test, empty) {
  intrusive::list<node> list;
  EXPECT_TRUE(list.begin() == list.end());
  EXPECT_FALSE(list.begin() != list.end());

  const intrusive::list<node>& clist = list;
  EXPECT_TRUE(clist.begin() == clist.end());
  EXPECT_FALSE(clist.begin() != clist.end());
}

TEST(iterator_test, default_ctor) {
  intrusive::list<node> list;
  node a(1);
  list.push_back(a);

  intrusive::list<node>::iterator i;
  intrusive::list<node>::const_iterator j;

  i = list.begin();
  j = list.begin();

  EXPECT_EQ(1, *i);
  EXPECT_EQ(1, *j);

  EXPECT_NE(list.end(), i);
  EXPECT_NE(list.end(), j);
}

TEST(iterator_test, copy_ctor) {
  intrusive::list<node> list;
  node a(1);
  list.push_back(a);

  intrusive::list<node>::iterator i = list.begin();

  intrusive::list<node>::iterator i2 = i;
  intrusive::list<node>::const_iterator j2 = i;

  EXPECT_EQ(1, *i2);
  EXPECT_EQ(1, *j2);

  EXPECT_NE(list.end(), i2);
  EXPECT_NE(list.end(), j2);
}

TEST(iterator_test, increment_1) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::iterator i = list.begin();
  EXPECT_EQ(1, *i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(2, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(3, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(4, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(5, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(list.end(), ++i);
}

TEST(iterator_test, increment_1c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::const_iterator i = list.begin();
  EXPECT_EQ(1, *i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(2, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(3, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(4, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(5, *++i);
  EXPECT_NE(list.end(), i);
  EXPECT_EQ(list.end(), ++i);
}

TEST(iterator_test, increment_2) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::iterator i = std::next(list.begin(), 1);
  EXPECT_EQ(2, *i);
  ++ ++i;
  EXPECT_EQ(4, *i);
}

TEST(iterator_test, increment_2c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::const_iterator i = std::next(list.begin(), 1);
  EXPECT_EQ(2, *i);
  ++ ++i;
  EXPECT_EQ(4, *i);
}

TEST(iterator_test, increment_3) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, b, c);
  list.push_front(a);

  intrusive::list<node>::iterator i = list.begin();
  EXPECT_EQ(1, *i);
  intrusive::list<node>::iterator j = i++;
  EXPECT_EQ(2, *i);
  EXPECT_EQ(1, *j);
  j = i++;
  EXPECT_EQ(3, *i);
  EXPECT_EQ(2, *j);
  j = i++;
  EXPECT_EQ(list.end(), i);
  EXPECT_EQ(3, *j);
}

TEST(iterator_test, increment_3c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, b, c);
  list.push_front(a);

  intrusive::list<node>::const_iterator i = list.begin();
  EXPECT_EQ(1, *i);
  intrusive::list<node>::const_iterator j = i++;
  EXPECT_EQ(2, *i);
  EXPECT_EQ(1, *j);
  j = i++;
  EXPECT_EQ(3, *i);
  EXPECT_EQ(2, *j);
  j = i++;
  EXPECT_EQ(list.end(), i);
  EXPECT_EQ(3, *j);
}

TEST(iterator_test, decrement_1) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::iterator i = list.end();
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(5, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(4, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(3, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(2, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(1, *--i);
  EXPECT_EQ(list.begin(), i);
}

TEST(iterator_test, decrement_1c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::const_iterator i = list.end();
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(5, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(4, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(3, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(2, *--i);
  EXPECT_NE(list.begin(), i);
  EXPECT_EQ(1, *--i);
  EXPECT_EQ(list.begin(), i);
}

TEST(iterator_test, decrement_2) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::iterator i = std::prev(list.end(), 2);
  EXPECT_EQ(4, *i);
  -- --i;
  EXPECT_EQ(2, *i);
}

TEST(iterator_test, decrement_2c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, c, d, e);
  mass_push_front(list, b, a);

  intrusive::list<node>::const_iterator i = std::prev(list.end(), 2);
  EXPECT_EQ(4, *i);
  -- --i;
  EXPECT_EQ(2, *i);
}

TEST(iterator_test, decrement_3) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, b, c);
  list.push_front(a);

  intrusive::list<node>::iterator i = list.end();
  intrusive::list<node>::iterator j = i--;
  EXPECT_EQ(3, *i);
  EXPECT_EQ(list.end(), j);
  j = i--;
  EXPECT_EQ(2, *i);
  EXPECT_EQ(3, *j);
  j = i--;
  EXPECT_EQ(1, *i);
  EXPECT_EQ(2, *j);
}

TEST(iterator_test, decrement_3c) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, b, c);
  list.push_front(a);

  intrusive::list<node>::const_iterator i = list.end();
  intrusive::list<node>::const_iterator j = i--;
  EXPECT_EQ(3, *i);
  EXPECT_EQ(list.end(), j);
  j = i--;
  EXPECT_EQ(2, *i);
  EXPECT_EQ(3, *j);
  j = i--;
  EXPECT_EQ(1, *i);
  EXPECT_EQ(2, *j);
}

TEST(iterator_test, swap) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2;
  node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  intrusive::list<node>::iterator i = std::next(list1.begin(), 2);
  intrusive::list<node>::iterator j = list2.begin();

  {
    using std::swap;
    swap(i, j);
  }

  list1.erase(j);
  list2.erase(i);

  expect_eq(list1, {1, 2});
  expect_eq(list2, {5, 6});
}

TEST(iterator_test, conversions) {
  intrusive::list<node> list;

  intrusive::list<node>::iterator i = list.end();
  intrusive::list<node>::const_iterator ci = i;

  EXPECT_TRUE(i == i);
  EXPECT_TRUE(ci == ci);
  EXPECT_TRUE(i == ci);
  EXPECT_TRUE(ci == i);
  EXPECT_FALSE(i != i);
  EXPECT_FALSE(ci != ci);
  EXPECT_FALSE(i != ci);
  EXPECT_FALSE(ci != i);

  EXPECT_TRUE(std::as_const(i) == i);
  EXPECT_TRUE(std::as_const(ci) == ci);
  EXPECT_TRUE(std::as_const(i) == ci);
  EXPECT_TRUE(std::as_const(ci) == i);
  EXPECT_FALSE(std::as_const(i) != i);
  EXPECT_FALSE(std::as_const(ci) != ci);
  EXPECT_FALSE(std::as_const(i) != ci);
  EXPECT_FALSE(std::as_const(ci) != i);

  EXPECT_TRUE(i == std::as_const(i));
  EXPECT_TRUE(ci == std::as_const(ci));
  EXPECT_TRUE(i == std::as_const(ci));
  EXPECT_TRUE(ci == std::as_const(i));
  EXPECT_FALSE(i != std::as_const(i));
  EXPECT_FALSE(ci != std::as_const(ci));
  EXPECT_FALSE(i != std::as_const(ci));
  EXPECT_FALSE(ci != std::as_const(i));

  EXPECT_TRUE(std::as_const(i) == std::as_const(i));
  EXPECT_TRUE(std::as_const(ci) == std::as_const(ci));
  EXPECT_TRUE(std::as_const(i) == std::as_const(ci));
  EXPECT_TRUE(std::as_const(ci) == std::as_const(i));
  EXPECT_FALSE(std::as_const(i) != std::as_const(i));
  EXPECT_FALSE(std::as_const(ci) != std::as_const(ci));
  EXPECT_FALSE(std::as_const(i) != std::as_const(ci));
  EXPECT_FALSE(std::as_const(ci) != std::as_const(i));
}

TEST(iterator_test, dereference_star) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, *i);
  EXPECT_EQ(&b, &*i);

  magic(*i);
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, *j);
  EXPECT_EQ(&c, &*j);

  magic(*j);
  expect_eq(list, {1, 42, 3});
}

TEST(iterator_test, dereference_star_const) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  const intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, *i);
  EXPECT_EQ(&b, &*i);

  magic(*i);
  expect_eq(list, {1, 42, 3});

  const intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, *j);
  EXPECT_EQ(&c, &*j);

  magic(*j);
  expect_eq(list, {1, 42, 3});
}

TEST(iterator_test, dereference_arrow) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  EXPECT_EQ(&b, i.operator->());

  magic(*i.operator->());
  expect_eq(list, {1, 42, 3});

  intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  EXPECT_EQ(&c, j.operator->());

  magic(*j.operator->());
  expect_eq(list, {1, 42, 3});
}

TEST(iterator_test, dereference_arrow_const) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  const intrusive::list<node>::iterator i = std::next(list.begin());
  EXPECT_EQ(2, i->value);
  EXPECT_EQ(&b, i.operator->());

  magic(*i.operator->());
  expect_eq(list, {1, 42, 3});

  const intrusive::list<node>::const_iterator j = std::next(list.begin(), 2);
  EXPECT_EQ(3, j->value);
  EXPECT_EQ(&c, j.operator->());

  magic(*j.operator->());
  expect_eq(list, {1, 42, 3});
}

TEST(iterator_test, list_modifications) {
  intrusive::list<node> list;

  intrusive::list<node>::iterator it1 = list.begin();
  intrusive::list<node>::iterator it2 = list.end();

  node a(1);
  list.push_back(a);

  intrusive::list<node>::iterator it3 = list.begin();
  intrusive::list<node>::iterator it4 = list.end();

  EXPECT_EQ(it1, it4);
  EXPECT_EQ(it2, it4);
  EXPECT_NE(it3, it4);

  --it1;
  --it2;

  EXPECT_EQ(it1, it3);
  EXPECT_EQ(it2, it3);
}