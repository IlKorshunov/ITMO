#pragma once

#include <set>

struct test_object {
  struct no_new_instances_guard;

  test_object() = delete;
  explicit test_object(int data);
  test_object(const test_object& other);
  ~test_object();

  test_object& operator=(const test_object& c);
  operator int() const;

private:
  int data;

  static std::set<const test_object*> instances;
};

struct test_object::no_new_instances_guard {
  no_new_instances_guard();

  no_new_instances_guard(const no_new_instances_guard&) = delete;
  no_new_instances_guard& operator=(const no_new_instances_guard&) = delete;

  ~no_new_instances_guard();

  void expect_no_instances() const;

private:
  std::set<const test_object*> old_instances;
};