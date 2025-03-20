#include <filesystem>
#include <fstream>

#include <gtest/gtest.h>

#include "lib/test.hpp"
#include "lib/util.hpp"

namespace fs = std::filesystem;

class EncodingTest : public NfsTest {};

TEST_F(EncodingTest, Display) {
  nfs.clear();
  nfs.create(ROOT_INO, "hello world", EntryType::FILE);

  std::set<std::string> expected_files{"hello world"};
  std::set<std::string> actual_files = list_directory({"."});

  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(EncodingTest, Spaces) {
  nfs.clear();

  std::fstream fs;
  fs.open("hello world", std::ios::out);
  ASSERT_FALSE(fs.fail());
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "hello world");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);
}

TEST_F(EncodingTest, Weird) {
  const std::string weird = "!@#$%^&*()-+ ";

  nfs.clear();

  std::fstream fs;
  fs.open(weird, std::ios::out);
  ASSERT_FALSE(fs.fail());
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, weird);
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);
}

TEST_F(EncodingTest, AllCharacters) {
  std::string name;
  for (unsigned char c = ' '; c <= 127u; c++) {
    if (c == '/') continue;
    name.append({(char)c});
  }

  std::fstream fs;
  fs.open(name, std::ios::out);
  ASSERT_FALSE(fs.fail());
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, name);
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);
}
