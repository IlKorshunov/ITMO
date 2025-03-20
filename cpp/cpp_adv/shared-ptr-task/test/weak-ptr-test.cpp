#include "shared-ptr.h"
#include "test-classes.h"

#include <gtest/gtest.h>

class weak_ptr_test : public ::testing::Test {
protected:
  test_object::no_new_instances_guard instances_guard;
};

TEST_F(weak_ptr_test, lock) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  shared_ptr<test_object> r = q.lock();
  EXPECT_TRUE(r == p);
  EXPECT_EQ(42, *r);
}

TEST_F(weak_ptr_test, lock_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  p.reset();
  instances_guard.expect_no_instances();
  shared_ptr<test_object> r = q.lock();
  EXPECT_FALSE(static_cast<bool>(r));
}

TEST_F(weak_ptr_test, lock_nullptr_2) {
  weak_ptr<test_object> q;
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, use_count) {
  shared_ptr<test_object> p1(new test_object(42));
  shared_ptr<test_object> p2 = p1;
  weak_ptr<test_object> q = p2;

  EXPECT_EQ(2, p1.use_count());
  EXPECT_EQ(3, q.lock().use_count());
}

TEST_F(weak_ptr_test, reset) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q1 = p;
  weak_ptr<test_object> q2 = p;
  q1.reset();
  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_TRUE(static_cast<bool>(p));
  EXPECT_TRUE(q2.lock() == p);
}

TEST_F(weak_ptr_test, reset_nullptr) {
  weak_ptr<test_object> q;
  EXPECT_FALSE(static_cast<bool>(q.lock()));
  q.reset();
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, make_shared) {
  shared_ptr<test_object> p = make_shared<test_object>(42);
  EXPECT_EQ(42, *p);
}

TEST_F(weak_ptr_test, make_shared_weak_ptr) {
  weak_ptr<test_object> p;
  {
    shared_ptr<test_object> q = make_shared<test_object>(42);
    p = q;
  }
  instances_guard.expect_no_instances();
}

TEST_F(weak_ptr_test, copy_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  weak_ptr<test_object> r = q;
  EXPECT_TRUE(r.lock() == p);
  EXPECT_TRUE(q.lock() == p);
}

TEST_F(weak_ptr_test, copy_ctor_nullptr) {
  weak_ptr<test_object> p;
  weak_ptr<test_object> q = p;
  EXPECT_FALSE(static_cast<bool>(p.lock()));
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, move_ctor) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;
  weak_ptr<test_object> r = std::move(q);
  EXPECT_TRUE(r.lock() == p);
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, move_ctor_nullptr) {
  weak_ptr<test_object> p;
  weak_ptr<test_object> q = p;
  EXPECT_FALSE(static_cast<bool>(p.lock()));
  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, copy_assignment_operator) {
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  shared_ptr<test_object> p2(new test_object(43));
  weak_ptr<test_object> q2 = p2;

  q1 = q2;

  EXPECT_TRUE(q1.lock() == p2);
  EXPECT_TRUE(q2.lock() == p2);
}

TEST_F(weak_ptr_test, copy_assignment_operator_from_nullptr) {
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  weak_ptr<test_object> q2;

  q1 = q2;

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, copy_assignment_operator_to_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2 = p;

  q1 = q2;

  EXPECT_TRUE(q1.lock() == p);
  EXPECT_TRUE(q2.lock() == p);
}

TEST_F(weak_ptr_test, copy_assignment_operator_nullptr) {
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2;

  q1 = q2;

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, copy_assignment_operator_self) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;

  q = q;

  EXPECT_TRUE(q.lock() == p);
}

TEST_F(weak_ptr_test, copy_assignment_operator_self_nullptr) {
  weak_ptr<test_object> q;

  q = q;

  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

TEST_F(weak_ptr_test, shared_assignment_operator) {
  shared_ptr<test_object> p1(new test_object(42));
  shared_ptr<test_object> p2(new test_object(43));
  weak_ptr<test_object> q = p1;

  q = p2;

  EXPECT_TRUE(q.lock() == p2);
}

TEST_F(weak_ptr_test, shared_assignment_operator_aliased) {
  test_object x(43);
  shared_ptr<test_object> p1(new test_object(42));
  shared_ptr<test_object> p2(p1, &x);
  weak_ptr<test_object> q = p1;

  q = p2;

  EXPECT_TRUE(q.lock() == p2);
}

TEST_F(weak_ptr_test, move_assignment_operator) {
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  shared_ptr<test_object> p2(new test_object(43));
  weak_ptr<test_object> q2 = p2;

  q1 = std::move(q2);

  EXPECT_TRUE(q1.lock() == p2);
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, move_assignment_operator_from_nullptr) {
  shared_ptr<test_object> p1(new test_object(42));
  weak_ptr<test_object> q1 = p1;
  weak_ptr<test_object> q2;

  q1 = std::move(q2);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, move_assignment_operator_to_nullptr) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2 = p;

  q1 = std::move(q2);

  EXPECT_TRUE(q1.lock() == p);
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, move_assignment_operator_nullptr) {
  weak_ptr<test_object> q1;
  weak_ptr<test_object> q2;

  q1 = std::move(q2);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_FALSE(static_cast<bool>(q2.lock()));
}

TEST_F(weak_ptr_test, move_assignment_operator_self) {
  shared_ptr<test_object> p(new test_object(42));
  weak_ptr<test_object> q = p;

  q = std::move(q);

  EXPECT_TRUE(q.lock() == p);
}

TEST_F(weak_ptr_test, move_assignment_operator_self_nullptr) {
  weak_ptr<test_object> q;

  q = std::move(q);

  EXPECT_FALSE(static_cast<bool>(q.lock()));
}

namespace {

struct base {};

struct derived : base {};

} // namespace

TEST_F(weak_ptr_test, shared_ctor_inheritance) {

  shared_ptr<derived> p(new derived());
  weak_ptr<base> q = p;

  EXPECT_TRUE(q.lock() == p);
}

TEST_F(weak_ptr_test, copy_ctor_inheritance) {
  shared_ptr<derived> p(new derived());
  weak_ptr<derived> q1 = p;
  weak_ptr<base> q2 = q1;

  EXPECT_TRUE(q1.lock() == p);
  EXPECT_TRUE(q2.lock() == p);
}

TEST_F(weak_ptr_test, move_ctor_inheritance) {
  shared_ptr<derived> p(new derived());
  weak_ptr<derived> q1 = p;
  weak_ptr<base> q2 = std::move(q1);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_TRUE(q2.lock() == p);
}

TEST_F(weak_ptr_test, copy_assignment_operator_inheritance) {
  shared_ptr<derived> p1(new derived());
  weak_ptr<derived> q1 = p1;
  shared_ptr<base> p2(new base());
  weak_ptr<base> q2 = p2;

  q2 = q1;

  EXPECT_TRUE(q1.lock() == p1);
  EXPECT_TRUE(q2.lock() == p1);
}

TEST_F(weak_ptr_test, move_assignment_operator_inheritance) {
  shared_ptr<derived> p1(new derived());
  weak_ptr<derived> q1 = p1;
  shared_ptr<base> p2(new base());
  weak_ptr<base> q2 = p2;

  q2 = std::move(q1);

  EXPECT_FALSE(static_cast<bool>(q1.lock()));
  EXPECT_TRUE(q2.lock() == p1);
}