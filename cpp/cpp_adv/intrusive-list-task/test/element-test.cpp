#include "intrusive_list.h"
#include "test-utils.h"

#include <utility>

TEST(element_test, default_ctor) {
  struct default_constructible_node : intrusive::list_element<> {};

  default_constructible_node node;
}

TEST(element_test, copy_ctor) {
  intrusive::list<copyable_node> list;
  copyable_node a(1);

  list.push_back(a);

  copyable_node b = a;

  list.push_back(b);

  EXPECT_EQ(&a, &list.front());
  EXPECT_EQ(&b, &list.back());

  expect_eq(list, {1, 1});
}

TEST(element_test, copy_ctor_empty) {
  copyable_node a(1);
  copyable_node b = a;

  intrusive::list<copyable_node> list;
  list.push_back(b);

  EXPECT_EQ(1, list.front());
}

TEST(element_test, copy_assignment) {
  intrusive::list<copyable_node> list1;
  copyable_node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<copyable_node> list2;
  copyable_node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  a = f;

  EXPECT_EQ(&f, &list2.back());
  expect_eq(list1, {2, 3});
  expect_eq(list2, {4, 5, 6});
}

TEST(element_test, copy_assignment_self) {
  intrusive::list<copyable_node> list;
  copyable_node a(1);
  list.push_back(a);

  a = a;

  EXPECT_FALSE(list.empty());
  EXPECT_EQ(&a, &list.front());
}

TEST(element_test, copy_assignment_empty) {
  copyable_node a(1), b(2);
  a = b;

  intrusive::list<copyable_node> list;
  list.push_back(a);

  EXPECT_EQ(2, list.front());
}

TEST(element_test, copy_assignment_empty_self) {
  copyable_node a(1);
  a = a;

  intrusive::list<copyable_node> list;
  list.push_back(a);

  EXPECT_EQ(1, list.front());
}

TEST(element_test, copy_assignment_to_empty) {
  intrusive::list<copyable_node> list;
  copyable_node a(1), b(2);

  list.push_back(b);

  a = b;

  EXPECT_FALSE(list.empty());
  EXPECT_EQ(&b, &list.front());

  list.push_back(a);

  EXPECT_EQ(&a, &list.back());

  expect_eq(list, {2, 2});
}

TEST(element_test, copy_assignment_from_empty) {
  intrusive::list<copyable_node> list;
  copyable_node a(1), b(2);

  list.push_back(a);

  a = b;

  EXPECT_TRUE(list.empty());

  list.push_back(a);
  list.push_back(b);

  expect_eq(list, {2, 2});
}

TEST(element_test, move_ctor) {
  intrusive::list<copyable_node> list;
  copyable_node a(1);

  list.push_back(a);

  copyable_node b = std::move(a);

  EXPECT_FALSE(list.empty());
  EXPECT_EQ(&b, &list.front());

  list.push_back(a);

  EXPECT_EQ(&a, &list.back());

  expect_eq(list, {1, 1});
}

TEST(element_test, move_ctor_empty) {
  copyable_node a(1);
  copyable_node b = std::move(a);

  intrusive::list<copyable_node> list;
  list.push_back(b);

  EXPECT_EQ(1, list.front());
}

TEST(element_test, move_assignment) {
  intrusive::list<copyable_node> list1;
  copyable_node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<copyable_node> list2;
  copyable_node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  a = std::move(f);

  EXPECT_EQ(&a, &list2.back());
  expect_eq(list1, {2, 3});
  expect_eq(list2, {4, 5, 6});
}

TEST(element_test, move_assignment_self) {
  intrusive::list<copyable_node> list;
  copyable_node a(1);
  list.push_back(a);

  a = std::move(a);

  EXPECT_FALSE(list.empty());
  EXPECT_EQ(&a, &list.front());
}

TEST(element_test, move_assignment_empty) {
  copyable_node a(1), b(2);
  a = std::move(b);

  intrusive::list<copyable_node> list;
  list.push_back(a);

  EXPECT_EQ(2, list.front());
}

TEST(element_test, move_assignment_empty_self) {
  copyable_node a(1);
  a = std::move(a);

  intrusive::list<copyable_node> list;
  list.push_back(a);

  EXPECT_EQ(1, list.front());
}

TEST(element_test, move_assignment_to_empty) {
  intrusive::list<copyable_node> list;
  copyable_node a(1), b(2);

  list.push_back(b);

  a = std::move(b);

  EXPECT_FALSE(list.empty());
  EXPECT_EQ(&a, &list.front());

  list.push_back(b);

  EXPECT_EQ(&b, &list.back());

  expect_eq(list, {2, 2});
}

TEST(element_test, move_assignment_from_empty) {
  intrusive::list<copyable_node> list;
  copyable_node a(1), b(2);

  list.push_back(a);

  a = std::move(b);

  EXPECT_TRUE(list.empty());

  list.push_back(a);
  list.push_back(b);

  expect_eq(list, {2, 2});
}