#pragma once

#include <exception>

class bad_function_call : public std::exception {
public:
  ~bad_function_call() override = default;

  const char* what() const noexcept override {
    return "bad function call";
  }
};