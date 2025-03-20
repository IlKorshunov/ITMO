#include "intrusive_list.h"
#include "test-utils.h"

#include <iterator>
#include <utility>

TEST(list_test, default_ctor) {
  intrusive::list<node> list;
  EXPECT_TRUE(list.empty());
  EXPECT_EQ(0, list.size());
}

TEST(list_test, push_back_lifetime_1) {
  intrusive::list<node> list;
  node a(1);
  list.push_back(a);
}

TEST(list_test, push_back_lifetime_2) {
  node a(1);
  intrusive::list<node> list;
  list.push_back(a);
}

TEST(list_test, push_front_lifetime_1) {
  intrusive::list<node> list;
  node a(1);
  list.push_front(a);
}

TEST(list_test, push_front_lifetime_2) {
  node a(1);
  intrusive::list<node> list;
  list.push_front(a);
}

TEST(list_test, push_back_many) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list, a, b, c, d, e);

  expect_eq(list, {1, 2, 3, 4, 5});
}

TEST(list_test, push_front_many) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_front(list, a, b, c, d, e);

  expect_eq(list, {5, 4, 3, 2, 1});
}

TEST(list_test, clear) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list, a, b, c, d);

  list.clear();
  EXPECT_TRUE(list.empty());
  EXPECT_EQ(0, list.size());
}

TEST(list_test, back_front) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  EXPECT_EQ(1, list.front());
  EXPECT_EQ(3, list.back());
  EXPECT_EQ(1, std::as_const(list).front());
  EXPECT_EQ(3, std::as_const(list).back());
}

TEST(list_test, back_front_ref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  list.front().value = 6;
  list.back().value = 7;

  expect_eq(list, {6, 2, 7});
}

TEST(list_test, back_front_cref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  EXPECT_TRUE(&list.front() == &std::as_const(list).front());
  EXPECT_TRUE(&list.back() == &std::as_const(list).back());
}

TEST(list_test, back_front_ncref) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  magic(std::as_const(list).front());
  magic(std::as_const(list).back());

  expect_eq(list, {1, 2, 3});
}

TEST(list_test, multiple_tags) {
  intrusive::list<multi_node, tag_a> list_a;
  intrusive::list<multi_node, tag_b> list_b;
  multi_node x(1), y(2), z(3);

  mass_push_back(list_a, x, y, z);
  mass_push_back(list_b, z, y, x);

  expect_eq(list_a, {1, 2, 3});
  expect_eq(list_b, {3, 2, 1});
}

TEST(list_test, multiple_tags_back_front) {
  intrusive::list<multi_node, tag_a> list_a;
  intrusive::list<multi_node, tag_b> list_b;
  multi_node x(1), y(2), z(3);

  mass_push_back(list_a, x, y, z);
  mass_push_back(list_b, z, y, x);

  EXPECT_EQ(1, list_a.front());
  EXPECT_EQ(3, list_a.back());
  EXPECT_EQ(1, std::as_const(list_a).front());
  EXPECT_EQ(3, std::as_const(list_a).back());
}

TEST(list_test, pop_back) {
  node a(1);
  intrusive::list<node> list;

  list.push_back(a);
  EXPECT_EQ(1, list.back());
  EXPECT_EQ(1, list.front());
  EXPECT_FALSE(list.empty());

  list.pop_back();
  EXPECT_TRUE(list.empty());
}

TEST(list_test, pop_front) {
  node a(1);
  intrusive::list<node> list;

  list.push_front(a);
  EXPECT_EQ(1, list.back());
  EXPECT_EQ(1, list.front());
  EXPECT_FALSE(list.empty());

  list.pop_front();
  EXPECT_TRUE(list.empty());
}

TEST(list_test, pop) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list, a, b, c, d);

  EXPECT_EQ(1, list.front());
  EXPECT_EQ(4, list.back());
  EXPECT_EQ(4, list.size());

  list.pop_front();
  EXPECT_EQ(2, list.front());
  EXPECT_EQ(4, list.back());
  EXPECT_EQ(3, list.size());

  list.pop_back();
  EXPECT_EQ(2, list.front());
  EXPECT_EQ(3, list.back());
  EXPECT_EQ(2, list.size());

  list.pop_front();
  EXPECT_EQ(3, list.front());
  EXPECT_EQ(3, list.back());
  EXPECT_EQ(1, list.size());
}

TEST(list_test, move_ctor) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2 = std::move(list1);

  EXPECT_TRUE(list1.empty());
  expect_eq(list2, {1, 2, 3});
}

TEST(list_test, move_ctor_empty) {
  intrusive::list<node> list1;
  intrusive::list<node> list2 = std::move(list1);

  EXPECT_TRUE(list1.empty());
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, move_assignment) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2;
  node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  list1 = std::move(list2);

  expect_eq(list1, {4, 5, 6});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, move_assignment_self) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  list = std::move(list);

  expect_eq(list, {1, 2, 3});
}

TEST(list_test, move_assignment_empty) {
  intrusive::list<node> list1, list2;

  list1 = std::move(list2);

  EXPECT_TRUE(list1.empty());
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, move_assignment_to_empty) {
  intrusive::list<node> list1;

  intrusive::list<node> list2;
  node a(1), b(2), c(3);
  mass_push_back(list2, a, b, c);

  list1 = std::move(list2);

  expect_eq(list1, {1, 2, 3});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, move_assignment_from_empty) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2;

  list1 = std::move(list2);

  EXPECT_TRUE(list1.empty());
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, swap) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2;
  node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  {
    using std::swap;
    swap(list1, list2);
  }

  expect_eq(list1, {4, 5, 6});
  expect_eq(list2, {1, 2, 3});
}

TEST(list_test, insert_front) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list, b, c, d);

  auto it1 = list.begin();
  auto it2 = list.insert(it1, a);

  EXPECT_EQ(1, *it2);
  EXPECT_EQ(2, *it1);
  EXPECT_NE(it1, it2);
  EXPECT_EQ(list.begin(), it2);

  ++it2;
  EXPECT_EQ(2, *it2);
  EXPECT_EQ(it1, it2);

  --it1;
  EXPECT_EQ(1, *it1);

  expect_eq(list, {1, 2, 3, 4});
}

TEST(list_test, insert_back) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list, a, b, c);

  auto it1 = list.end();
  auto it2 = list.insert(it1, d);

  EXPECT_EQ(4, *it2);
  --it2;
  EXPECT_EQ(3, *it2);
  ++it2;
  EXPECT_EQ(4, *it2);
  ++it2;
  EXPECT_EQ(it1, it2);
  --it1;
  EXPECT_EQ(4, *it1);
}

TEST(list_test, insert_middle) {
  intrusive::list<node> list;
  node a(1), b(2), c(3), d(4);

  list.push_back(a);
  list.push_back(b);
  list.push_back(d);

  auto it1 = std::next(list.begin(), 2);
  auto it2 = list.insert(it1, c);

  EXPECT_EQ(3, *it2);
  --it2;
  EXPECT_EQ(2, *it2);
  ++it2;
  EXPECT_EQ(3, *it2);
  ++it2;
  EXPECT_EQ(4, *it2);
  EXPECT_EQ(it1, it2);
  --it1;
  EXPECT_EQ(3, *it1);
}

TEST(list_test, reinsert_to_same_list) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  auto b_iter = std::next(list.begin());

  list.insert(b_iter, b);
  expect_eq(list, {1, 2, 3});

  list.insert(b_iter, a);
  expect_eq(list, {1, 2, 3});

  list.insert(b_iter, c);
  expect_eq(list, {1, 3, 2});
}

TEST(list_test, reinsert_to_another_list) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3);
  mass_push_back(list1, a, b, c);

  intrusive::list<node> list2;
  node d(4), e(5), f(6);
  mass_push_back(list2, d, e, f);

  auto b_iter = std::next(list1.begin());
  auto e_iter = std::next(list2.begin());

  list1.insert(b_iter, f);
  expect_eq(list1, {1, 6, 2, 3});
  expect_eq(list2, {4, 5});

  list2.insert(e_iter, b);
  expect_eq(list1, {1, 6, 3});
  expect_eq(list2, {4, 2, 5});

  list2.insert(b_iter, f);
  expect_eq(list1, {1, 3});
  expect_eq(list2, {4, 6, 2, 5});
}

TEST(list_test, erase_front) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  auto erased_it = list.begin();
  auto it = list.erase(erased_it);

  EXPECT_EQ(2, *it);
  EXPECT_EQ(list.begin(), it);

  expect_eq(list, {2, 3});
}

TEST(list_test, erase_back) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  auto erased_it = std::prev(list.end());
  auto it = list.erase(erased_it);

  EXPECT_EQ(list.end(), it);
  --it;
  EXPECT_EQ(2, *it);

  expect_eq(list, {1, 2});
}

TEST(list_test, erase_middle) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);
  mass_push_back(list, a, b, c);

  auto erased_it = std::next(list.begin(), 1);
  auto it = list.erase(erased_it);

  EXPECT_EQ(3, *it);

  --it;
  EXPECT_EQ(1, *it);
  EXPECT_EQ(list.begin(), it);

  ++it;
  EXPECT_EQ(3, *it);

  expect_eq(list, {1, 3});
}

TEST(list_test, insert_and_erase_1) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(b);
  list.push_back(c);

  auto it1 = list.insert(list.begin(), a);
  auto it2 = std::next(it1);

  list.erase(it1);
  EXPECT_EQ(list.begin(), it2);

  expect_eq(list, {2, 3});
}

TEST(list_test, insert_and_erase_2) {
  intrusive::list<node> list;
  node a(1), b(2), c(3);

  list.push_back(b);
  list.push_back(c);

  auto it1 = list.insert(list.begin(), a);
  auto it2 = std::next(it1);

  list.erase(it2);

  EXPECT_EQ(list.begin(), it1);
  ++it1;
  EXPECT_EQ(3, *it1);

  expect_eq(list, {1, 3});
}

TEST(list_test, splice_begin_begin) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.begin(), list2, list2.begin(), std::next(list2.begin(), 2));

  expect_eq(list1, {5, 6, 1, 2, 3, 4});
  expect_eq(list2, {7, 8});
}

TEST(list_test, splice_begin_middle) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.begin(), list2, std::next(list2.begin()), std::next(list2.begin(), 2));

  expect_eq(list1, {6, 1, 2, 3, 4});
  expect_eq(list2, {5, 7, 8});
}

TEST(list_test, splice_begin_end) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.begin(), list2, std::next(list2.begin(), 2), list2.end());

  expect_eq(list1, {7, 8, 1, 2, 3, 4});
  expect_eq(list2, {5, 6});
}

TEST(list_test, splice_begin_whole) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.begin(), list2, list2.begin(), list2.end());

  expect_eq(list1, {5, 6, 7, 8, 1, 2, 3, 4});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, splice_begin_empty) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.begin(), list2, std::next(list2.begin(), 2), std::next(list2.begin(), 2));

  expect_eq(list1, {1, 2, 3, 4});
  expect_eq(list2, {5, 6, 7, 8});
}

TEST(list_test, splice_middle_begin) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(std::next(list1.begin(), 2), list2, list2.begin(), std::next(list2.begin(), 2));

  expect_eq(list1, {1, 2, 5, 6, 3, 4});
  expect_eq(list2, {7, 8});
}

TEST(list_test, splice_middle_middle) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(std::next(list1.begin(), 2), list2, std::next(list2.begin()), std::next(list2.begin(), 3));

  expect_eq(list1, {1, 2, 6, 7, 3, 4});
  expect_eq(list2, {5, 8});
}

TEST(list_test, splice_middle_end) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(std::next(list1.begin(), 2), list2, std::next(list2.begin(), 2), list2.end());

  expect_eq(list1, {1, 2, 7, 8, 3, 4});
  expect_eq(list2, {5, 6});
}

TEST(list_test, splice_middle_whole) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(std::next(list1.begin(), 2), list2, list2.begin(), list2.end());

  expect_eq(list1, {1, 2, 5, 6, 7, 8, 3, 4});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, splice_middle_empty) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(std::next(list1.begin(), 2), list2, std::next(list2.begin(), 2), std::next(list2.begin(), 2));

  expect_eq(list1, {1, 2, 3, 4});
  expect_eq(list2, {5, 6, 7, 8});
}

TEST(list_test, splice_end_begin) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, list2.begin(), std::next(list2.begin(), 2));

  expect_eq(list1, {1, 2, 3, 4, 5, 6});
  expect_eq(list2, {7, 8});
}

TEST(list_test, splice_end_middle) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, std::next(list2.begin()), std::next(list2.begin(), 3));

  expect_eq(list1, {1, 2, 3, 4, 6, 7});
  expect_eq(list2, {5, 8});
}

TEST(list_test, splice_end_end) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, std::next(list2.begin(), 2), list2.end());

  expect_eq(list1, {1, 2, 3, 4, 7, 8});
  expect_eq(list2, {5, 6});
}

TEST(list_test, splice_end_whole) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, list2.begin(), list2.end());

  expect_eq(list1, {1, 2, 3, 4, 5, 6, 7, 8});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, splice_end_empty) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, std::next(list2.begin(), 2), std::next(list2.begin(), 2));

  expect_eq(list1, {1, 2, 3, 4});
  expect_eq(list2, {5, 6, 7, 8});
}

TEST(list_test, splice_empty_begin) {
  intrusive::list<node> list1, list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, list2.begin(), std::next(list2.begin(), 2));

  expect_eq(list1, {5, 6});
  expect_eq(list2, {7, 8});
}

TEST(list_test, splice_empty_middle) {
  intrusive::list<node> list1, list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, std::next(list2.begin(), 1), std::next(list2.begin(), 3));

  expect_eq(list1, {6, 7});
  expect_eq(list2, {5, 8});
}

TEST(list_test, splice_empty_end) {
  intrusive::list<node> list1, list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, std::next(list2.begin(), 2), list2.end());

  expect_eq(list1, {7, 8});
  expect_eq(list2, {5, 6});
}

TEST(list_test, splice_empty_whole) {
  intrusive::list<node> list1, list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, list2.begin(), list2.end());

  expect_eq(list1, {5, 6, 7, 8});
  EXPECT_TRUE(list2.empty());
}

TEST(list_test, splice_empty_empty) {
  intrusive::list<node> list1, list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  list1.splice(list1.end(), list2, list2.begin(), list2.begin());

  EXPECT_TRUE(list1.empty());
  expect_eq(list2, {5, 6, 7, 8});
}

TEST(list_test, splice_self) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4), e(5);
  mass_push_back(list1, a, b, c, d, e);

  list1.splice(std::next(list1.begin()), list1, std::next(list1.begin(), 2), std::prev(list1.end()));

  expect_eq(list1, {1, 3, 4, 2, 5});
}

TEST(list_test, splice_iterators) {
  intrusive::list<node> list1;
  node a(1), b(2), c(3), d(4);
  mass_push_back(list1, a, b, c, d);

  intrusive::list<node> list2;
  node e(5), f(6), g(7), h(8);
  mass_push_back(list2, e, f, g, h);

  intrusive::list<node>::const_iterator i = std::next(list1.begin(), 2);
  intrusive::list<node>::const_iterator j = std::next(list2.begin());
  intrusive::list<node>::const_iterator k = std::prev(list2.end());

  list1.splice(i, list2, j, k);

  expect_eq(list1, {1, 2, 6, 7, 3, 4});
  expect_eq(list2, {5, 8});

  EXPECT_EQ(3, *i);
  EXPECT_EQ(6, *j);
  EXPECT_EQ(8, *k);

  EXPECT_EQ(7, *std::prev(i));
  EXPECT_EQ(2, *std::prev(j));
  EXPECT_EQ(5, *std::prev(k));
}