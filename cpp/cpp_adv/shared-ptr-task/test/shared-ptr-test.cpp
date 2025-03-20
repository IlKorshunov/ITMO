#include "shared-ptr.h"

#include "test-classes.h"

#include <gtest/gtest.h>

class shared_ptr_test : public ::testing::Test {
protected:
  test_object::no_new_instances_guard instances_guard;
};

TEST_F(shared_ptr_test, default_ctor) {
  shared_ptr<test_object> p;
  EXPECT_EQ(nullptr, p.get());
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, ptr_ctor) {
  test_object* p = new test_object(42);
  shared_ptr<test_object> q(p);
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_EQ(p, q.get());
  EXPECT_EQ(42, *q);
}

TEST_F(shared_ptr_test, ptr_ctor_nullptr) {
  shared_ptr<test_object> p(nullptr);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_EQ(0, p.use_count());
}

TEST_F(shared_ptr_test, ptr_ctor_non_empty_nullptr) {
  shared_ptr<test_object> p(static_cast<test_object*>(nullptr));
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_EQ(1, p.use_count());
}

TEST_F(shared_ptr_test, const_dereferencing) {
  const shared_ptr<test_object> p(new test_object(42));
  EXPECT_EQ(42, *p);
  EXPECT_EQ(42, p->operator int());
}

TEST_F(shared_ptr_test, reset) {
  shared_ptr<test_object> q(new test_object(42));
  EXPECT_TRUE(static_cast<bool>(q));
  q.reset();
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, reset_nullptr) {
  shared_ptr<test_object> q;
  EXPECT_FALSE(static_cast<bool>(q));
  q.reset();
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, reset_ptr) {
  shared_ptr<test_object> q(new test_object(42));
  EXPECT_TRUE(static_cast<bool>(q));
  q.reset(new test_object(43));
  EXPECT_EQ(43, *q);
}

TEST_F(shared_ptr_test, copy_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  EXPECT_EQ(1, p.use_count());
  shared_ptr<test_object> q = p;
  EXPECT_TRUE(static_cast<bool>(p));
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_TRUE(p == q);
  EXPECT_EQ(42, *p);
  EXPECT_EQ(42, *q);
  EXPECT_EQ(2, q.use_count());
}

TEST_F(shared_ptr_test, copy_ctor_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q = p;
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, move_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_TRUE(static_cast<bool>(q));
  EXPECT_EQ(42, *q);
}

TEST_F(shared_ptr_test, move_ctor_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, copy_assignment_operator) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q(new test_object(43));
  p = q;
  EXPECT_EQ(43, *p);
  EXPECT_TRUE(p == q);
}

TEST_F(shared_ptr_test, copy_assignment_operator_from_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q;
  p = q;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, copy_assignment_operator_to_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q(new test_object(43));
  p = q;
  EXPECT_EQ(43, *p);
  EXPECT_TRUE(p == q);
}

TEST_F(shared_ptr_test, copy_assignment_operator_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q;
  p = q;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, copy_assignment_operator_self) {
  shared_ptr<test_object> p(new test_object(42));
  p = p;
  EXPECT_EQ(42, *p);
}

TEST_F(shared_ptr_test, copy_assignment_operator_self_nullptr) {
  shared_ptr<test_object> p;
  p = p;
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, move_assignment_operator) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q(new test_object(43));
  p = std::move(q);
  EXPECT_EQ(43, *p);
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, move_assignment_operator_from_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<test_object> q;
  p = std::move(q);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, move_assignment_operator_to_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q(new test_object(43));
  p = std::move(q);
  EXPECT_EQ(43, *p);
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, move_assignment_operator_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<test_object> q;
  p = std::move(q);
  EXPECT_FALSE(static_cast<bool>(p));
  EXPECT_FALSE(static_cast<bool>(q));
}

TEST_F(shared_ptr_test, move_assignment_operator_self) {
  shared_ptr<test_object> p(new test_object(42));
  p = std::move(p);
  EXPECT_EQ(42, *p);
}

TEST_F(shared_ptr_test, move_assignment_operator_self_nullptr) {
  shared_ptr<test_object> p;
  p = std::move(p);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, make_shared) {
  shared_ptr<test_object> p = make_shared<test_object>(42);
  EXPECT_EQ(42, *p);
}

TEST_F(shared_ptr_test, make_shared_forwarding) {
  struct pair {
    pair(int& x, double&& y) : x(x), y(y) {}

    int& x;
    double y;
  };

  int x = 42;
  shared_ptr<pair> p = make_shared<pair>(x, 3.14);
  EXPECT_EQ(p->x, 42);
  EXPECT_EQ(&p->x, &x);
  EXPECT_EQ(p->y, 3.14);
}

TEST_F(shared_ptr_test, ptr_ctor_inheritance) {
  bool deleted = false;
  { shared_ptr<destruction_tracker_base> p(new destruction_tracker(&deleted)); }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, reset_ptr_inheritance) {
  bool deleted = false;
  {
    shared_ptr<destruction_tracker_base> p;
    p.reset(new destruction_tracker(&deleted));
  }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, custom_deleter) {
  bool deleted = false;
  { shared_ptr<test_object> p(new test_object(42), tracking_deleter<test_object>(&deleted)); }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, custom_deleter_reset) {
  bool deleted;
  {
    shared_ptr<test_object> p;
    p.reset(new test_object(42), tracking_deleter<test_object>(&deleted));
  }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, aliasing_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  int x;
  shared_ptr<int> q(p, &x);
  EXPECT_EQ(2, p.use_count());
  EXPECT_EQ(2, q.use_count());
  EXPECT_EQ(&x, q.get());
}

TEST_F(shared_ptr_test, aliasing_ctor_nullptr_non_empty) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<int> q(p, nullptr);
  EXPECT_EQ(2, p.use_count());
  EXPECT_EQ(2, q.use_count());
  EXPECT_EQ(nullptr, q.get());
}

TEST_F(shared_ptr_test, aliasing_move_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  int x;
  shared_ptr<int> q(std::move(p), &x);
  EXPECT_EQ(0, p.use_count());
  EXPECT_EQ(1, q.use_count());
  EXPECT_EQ(nullptr, p.get());
  EXPECT_EQ(&x, q.get());
}

TEST_F(shared_ptr_test, aliasing_move_ctor_nullptr_non_empty) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<int> q(std::move(p), nullptr);
  EXPECT_EQ(0, p.use_count());
  EXPECT_EQ(1, q.use_count());
  EXPECT_EQ(nullptr, p.get());
  EXPECT_EQ(nullptr, q.get());
}

TEST_F(shared_ptr_test, copy_ctor_const) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q = p;
  EXPECT_EQ(42, *q);
}

TEST_F(shared_ptr_test, move_ctor_const) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q = std::move(p);
  EXPECT_EQ(42, *q);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, copy_assignment_operator_const) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q(new test_object(43));
  q = p;
  EXPECT_EQ(42, *q);
  EXPECT_EQ(42, *p);
}

TEST_F(shared_ptr_test, copy_assignment_operator_const_to_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q;
  q = p;
  EXPECT_EQ(42, *q);
  EXPECT_EQ(42, *p);
}

TEST_F(shared_ptr_test, copy_assignment_operator_const_from_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<const test_object> q(new test_object(43));
  q = p;
  EXPECT_FALSE(static_cast<bool>(q));
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, move_assignment_operator_const) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q(new test_object(43));
  q = std::move(p);
  EXPECT_EQ(42, *q);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, move_assignment_operator_const_to_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  shared_ptr<const test_object> q;
  q = std::move(p);
  EXPECT_EQ(42, *q);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, move_assignment_operator_const_from_nullptr) {
  shared_ptr<test_object> p;
  shared_ptr<const test_object> q(new test_object(43));
  q = std::move(p);
  EXPECT_FALSE(static_cast<bool>(q));
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST_F(shared_ptr_test, copy_ctor_inheritance) {
  bool deleted = false;
  {
    destruction_tracker* ptr = new destruction_tracker(&deleted);
    shared_ptr<destruction_tracker> d(ptr);
    {
      shared_ptr<destruction_tracker_base> b = d;
      EXPECT_EQ(ptr, b.get());
      EXPECT_EQ(ptr, d.get());
    }
    EXPECT_FALSE(deleted);
  }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, move_ctor_inheritance) {
  bool deleted = false;
  {
    destruction_tracker* ptr = new destruction_tracker(&deleted);
    shared_ptr<destruction_tracker> d(ptr);
    {
      shared_ptr<destruction_tracker_base> b = std::move(d);
      EXPECT_EQ(ptr, b.get());
      EXPECT_FALSE(static_cast<bool>(d));
    }
    EXPECT_TRUE(deleted);
    deleted = false;
  }
  EXPECT_FALSE(deleted);
}

TEST_F(shared_ptr_test, copy_assignment_operator_inheritance) {
  bool deleted = false;
  {
    destruction_tracker* ptr = new destruction_tracker(&deleted);
    shared_ptr<destruction_tracker> d(ptr);
    {
      shared_ptr<destruction_tracker_base> b(new destruction_tracker_base());
      b = d;
      EXPECT_EQ(ptr, b.get());
      EXPECT_EQ(ptr, d.get());
    }
    EXPECT_FALSE(deleted);
  }
  EXPECT_TRUE(deleted);
}

TEST_F(shared_ptr_test, move_assignment_operator_inheritance) {
  bool deleted = false;
  {
    destruction_tracker* ptr = new destruction_tracker(&deleted);
    shared_ptr<destruction_tracker> d(ptr);
    {
      shared_ptr<destruction_tracker_base> b(new destruction_tracker_base());
      b = std::move(d);
      EXPECT_EQ(ptr, b.get());
      EXPECT_FALSE(static_cast<bool>(d));
    }
    EXPECT_TRUE(deleted);
    deleted = false;
  }
  EXPECT_FALSE(deleted);
}

TEST_F(shared_ptr_test, equivalence) {
  shared_ptr<test_object> p1(new test_object(42));
  shared_ptr<test_object> p2(new test_object(43));

  EXPECT_FALSE(p1 == p2);
  EXPECT_TRUE(p1 != p2);
}

TEST_F(shared_ptr_test, equivalence_aliasing) {
  shared_ptr<test_object> p1(new test_object(42));
  shared_ptr<test_object> p2(new test_object(43));
  shared_ptr<test_object> q1(p2, p1.get());
  shared_ptr<test_object> q2(p1, p2.get());

  EXPECT_TRUE(p1 == q1);
  EXPECT_FALSE(p1 != q1);

  EXPECT_TRUE(p2 == q2);
  EXPECT_FALSE(p2 != q2);

  EXPECT_FALSE(p1 == q2);
  EXPECT_TRUE(p1 != q2);

  EXPECT_FALSE(p2 == q1);
  EXPECT_TRUE(p2 != q1);
}

TEST_F(shared_ptr_test, equivalence_self) {
  shared_ptr<test_object> p(new test_object(42));

  EXPECT_TRUE(p == p);
  EXPECT_FALSE(p != p);
}

TEST_F(shared_ptr_test, equivalence_nullptr) {
  shared_ptr<test_object> p;

  EXPECT_TRUE(p == nullptr);
  EXPECT_FALSE(p != nullptr);
  EXPECT_TRUE(nullptr == p);
  EXPECT_FALSE(nullptr != p);
}