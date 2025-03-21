#ifndef NETWORKFS_TEST_TEST_HPP
#define NETWORKFS_TEST_TEST_HPP

#include <filesystem>

#include <gtest/gtest.h>

#include "nfs.hpp"
#include "util.hpp"

namespace fs = std::filesystem;

class NfsTest : public testing::Test {
public:
  fs::path previous_path;
  NfsBucket nfs;

  NfsTest() : nfs() {};

protected:
  void SetUp() override {
    nfs.initialize();
    std::cerr << "Token for this run: " << nfs.token() << std::endl;
    previous_path = fs::current_path();
    fs::current_path(TEST_ROOT);
  }

  void TearDown() override {
    fs::current_path(previous_path);
    nfs.unmount(true);
  }
};

#endif
