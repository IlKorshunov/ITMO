#include <filesystem>
#include <fstream>

#include <gtest/gtest.h>

#include "lib/test.hpp"
#include "lib/util.hpp"

namespace fs = std::filesystem;

class BaseTest : public NfsTest {};

TEST_F(BaseTest, ListDefaultFiles) {
  std::set<std::string> expected_files{"file1", "file2"};
  std::set<std::string> actual_files = list_directory({"."});

  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, ListEmpty) {
  nfs.clear();

  std::set<std::string> expected_files{};
  std::set<std::string> actual_files = list_directory({"."});

  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, ListFull) {
  nfs.clear();

  std::set<std::string> expected_files;
  for (int i = 0; i < 16; i++) {
    expected_files.insert("test" + std::to_string(i));
  }

  for (const auto& entry: expected_files) {
    nfs.create(ROOT_INO, entry, EntryType::FILE);
  }

  std::set<std::string> actual_files = list_directory({"."});
  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, ListNested) {
  ino_t outer = nfs.create(ROOT_INO, "outer", EntryType::DIRECTORY).ino;
  ino_t inner = nfs.create(outer, "inner", EntryType::DIRECTORY).ino;
  nfs.create(inner, "file", EntryType::FILE);

  std::set<std::string> expected_files{"file"};
  std::set<std::string> actual_files = list_directory({"outer/inner"});
  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, FileTypes) {
  nfs.create(ROOT_INO, "dir", EntryType::DIRECTORY);
  
  ASSERT_TRUE(fs::is_regular_file({"file1"}));
  ASSERT_TRUE(fs::is_regular_file({"file2"}));
  ASSERT_TRUE(fs::is_directory({"dir"}));
}

TEST_F(BaseTest, NotFound) {
  ASSERT_FALSE(fs::exists({"abcde"}));
}

TEST_F(BaseTest, CreateFile) {
  std::fstream fs;

  fs.open("test", std::ios::out);
  ASSERT_FALSE(fs.fail());
  fs.close();
  ASSERT_FALSE(fs.fail());
  
  lookup_response response = nfs.lookup(ROOT_INO, "test");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);
}

TEST_F(BaseTest, CreateDirectories) {
  ASSERT_NO_THROW(fs::create_directory("test"));
  
  lookup_response response = nfs.lookup(ROOT_INO, "test");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::DIRECTORY);

  ASSERT_NO_THROW(fs::create_directory("test/nested"));

  ASSERT_TRUE(fs::is_directory("test/nested"));

  lookup_response response_nested = nfs.lookup(response.ino, "nested");
  ASSERT_EQ(response_nested.status, 0);
  ASSERT_EQ(response_nested.entry_type, EntryType::DIRECTORY);
}

TEST_F(BaseTest, TooManyFiles) {
  nfs.clear();

  std::set<std::string> expected_files;
  for (int i = 0; i < 16; i++) {
    expected_files.insert("test" + std::to_string(i));
  }

  ASSERT_NO_THROW({
    for (const auto& entry: expected_files) {
      fs::create_directory({entry});
    }
  });

  ASSERT_THROW(
    fs::create_directory("test17"),
    fs::filesystem_error
  );

  std::set<std::string> actual_files = list_directory({"."});
  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, LongName) {
  std::fstream fs;
  std::string name(255, 'a');

  fs.open(name, std::ios::out);
  ASSERT_FALSE(fs.fail());
  fs.close();
  ASSERT_FALSE(fs.fail());
  
  lookup_response response = nfs.lookup(ROOT_INO, name);
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);
}

TEST_F(BaseTest, LongerName) {
  nfs.clear();

  std::fstream fs;
  std::string name(256, 'a');

  fs.open(name, std::ios::out);
  ASSERT_TRUE(fs.fail());

  list_response response = nfs.list(ROOT_INO);
  ASSERT_EQ(response.entries_count, 0);
}

TEST_F(BaseTest, RemoveFile) {
  ASSERT_NO_THROW(fs::remove("file1"));
  
  list_response response = nfs.list(ROOT_INO);
  ASSERT_EQ(response.entries_count, 1);
  ASSERT_EQ(std::string(response.entries[0].name), "file2");

  std::set<std::string> expected_files{"file2"};
  std::set<std::string> actual_files = list_directory({"."});
  ASSERT_EQ(actual_files, expected_files);
}

TEST_F(BaseTest, RemoveDirectory) {
  nfs.clear();

  nfs.create(ROOT_INO, "directory", EntryType::DIRECTORY);

  ASSERT_NO_THROW(fs::remove("directory"));

  list_response response = nfs.list(ROOT_INO);
  ASSERT_EQ(response.entries_count, 0);
}

TEST_F(BaseTest, RemoveInDirectory) {
  ino_t ino = nfs.create(ROOT_INO, "directory", EntryType::DIRECTORY).ino;
  nfs.create(ino, "file", EntryType::FILE);

  ASSERT_NO_THROW(fs::remove("directory/file"));

  list_response response = nfs.list(ino);
  ASSERT_EQ(response.entries_count, 0);
}

TEST_F(BaseTest, RemoveDirectoryAfterEmptying) {
  nfs.clear();

  ino_t ino = nfs.create(ROOT_INO, "directory", EntryType::DIRECTORY).ino;
  nfs.create(ino, "file", EntryType::FILE);

  ASSERT_NO_THROW(fs::remove("directory/file"));
  ASSERT_NO_THROW(fs::remove("directory"));

  list_response response = nfs.list(ROOT_INO);
  ASSERT_EQ(response.entries_count, 0);
}

TEST_F(BaseTest, RemoveNonEmptyDirectory) {
  nfs.clear();

  ino_t ino = nfs.create(ROOT_INO, "directory", EntryType::DIRECTORY).ino;
  nfs.create(ino, "file", EntryType::FILE);

  ASSERT_THROW(fs::remove("directory"), fs::filesystem_error);

  list_response response = nfs.list(ROOT_INO);
  ASSERT_EQ(response.entries_count, 1);
  ASSERT_EQ(std::string(response.entries[0].name), "directory");

  std::set<std::string> expected_files{"directory"};
  std::set<std::string> actual_files = list_directory({"."});
  ASSERT_EQ(actual_files, expected_files);
}
