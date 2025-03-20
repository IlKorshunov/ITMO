#include <filesystem>
#include <fcntl.h>
#include <fstream>
#include <sstream>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <gtest/gtest.h>

#include "lib/test.hpp"
#include "lib/util.hpp"

namespace fs = std::filesystem;

class FileTest : public NfsTest {};

TEST_F(FileTest, Read) {
  std::fstream fs;
  fs.open("file1");
  ASSERT_FALSE(fs.fail());

  std::stringstream buffer;
  buffer << fs.rdbuf();

  ASSERT_EQ(buffer.str(), "hello world from file1");

  fs.close();
}

TEST_F(FileTest, ReadLong) {
  nfs.clear();
  ino_t file = nfs.create(ROOT_INO, "file", EntryType::FILE).ino;
  std::string content(512, 'a');
  nfs.write(file, content);

  std::fstream fs;
  fs.open("file");
  ASSERT_FALSE(fs.fail());

  std::stringstream buffer;
  buffer << fs.rdbuf();

  ASSERT_EQ(buffer.str(), content);

  fs.close();
}

TEST_F(FileTest, ReadSeek) {
  std::fstream fs;
  fs.open("file1");
  ASSERT_FALSE(fs.fail());

  fs.seekg(11, std::ios_base::beg);

  std::stringstream buffer;
  buffer << fs.rdbuf();

  ASSERT_EQ(buffer.str(), " from file1");

  fs.close();
}

TEST_F(FileTest, ReadChunked) {
  std::fstream fs;
  fs.open("file1");
  ASSERT_FALSE(fs.fail());

  char out[128];

  memset(out, 0, sizeof(out));
  fs.read(out, 5);
  ASSERT_STREQ(out, "hello");

  memset(out, 0, sizeof(out));
  fs.read(out, 6);
  ASSERT_STREQ(out, " world");

  memset(out, 0, sizeof(out));
  fs.read(out, 127);
  ASSERT_STREQ(out, " from file1");

  ASSERT_TRUE(fs.eof());
  fs.close();
}

TEST_F(FileTest, ReadSpecial) {
  nfs.clear();

  std::string content;
  for (unsigned char c = 0u; c <= 127u; c++) {
    content.append({(char)c});
  }

  ino_t file = nfs.create(ROOT_INO, "file", EntryType::FILE).ino;
  nfs.write(file, content);

  std::fstream fs;
  fs.open("file");
  ASSERT_FALSE(fs.fail());

  std::stringstream buffer;
  buffer << fs.rdbuf();

  ASSERT_EQ(buffer.str(), content);

  fs.close();
}

TEST_F(FileTest, WriteNewFile) {
  nfs.clear();

  std::string content = "test-file-content";

  std::fstream fs;
  fs.open("file", std::ios::out);
  ASSERT_FALSE(fs.fail());

  fs << content;
  fs.close();

  lookup_response response = nfs.lookup(ROOT_INO, "file");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, content);
}

TEST_F(FileTest, WriteReplacing) {
  std::fstream fs;
  fs.open("file1", std::ios::in | std::ios::out);
  ASSERT_FALSE(fs.fail());
  
  fs << "replaced-text-in-"; 
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "file1");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  std::string expected_content = "replaced-text-in-file1";

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, expected_content);
}

TEST_F(FileTest, WriteReplacing2) {
  std::fstream fs;
  fs.open("file1", std::ios::in | std::ios::out);
  ASSERT_FALSE(fs.fail());

  std::string content = "a-long-time-ago-in-a-galaxy-far-far-away";

  fs << content;
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "file1");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, content);
}

TEST_F(FileTest, WriteTruncating) {
  std::fstream fs;
  fs.open("file1", std::ios::out);
  ASSERT_FALSE(fs.fail());

  std::string content = "my-new-file";
  
  fs << content;
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "file1");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, content);
}

TEST_F(FileTest, WriteAppending) {
  nfs.clear();
  ino_t ino = nfs.create(ROOT_INO, "file", EntryType::FILE).ino;
  nfs.write(ino, "hello");

  std::fstream fs;
  fs.open("file", std::ios::out | std::ios::app);
  ASSERT_FALSE(fs.fail());
  
  fs << "-world"; 
  fs.close();
  ASSERT_FALSE(fs.fail());

  std::string expected_content = "hello-world";

  read_response file = nfs.read(ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, expected_content);
}

TEST_F(FileTest, WriteSeek) {
  nfs.clear();
  ino_t ino = nfs.create(ROOT_INO, "file", EntryType::FILE).ino;
  nfs.write(ino, "hello-world");

  std::fstream fs;
  fs.open("file", std::ios::in | std::ios::out | std::ios::binary);
  ASSERT_FALSE(fs.fail());

  fs.seekg(-5, std::ios_base::end);
  
  fs << "fseek-implementation"; 
  fs.close();
  ASSERT_FALSE(fs.fail());

  std::string expected_content = "hello-fseek-implementation";

  read_response file = nfs.read(ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, expected_content);
}

TEST_F(FileTest, Synchronize) {
  nfs.clear();

  int fd = open("file", O_RDWR | O_CREAT);
  ASSERT_NE(fd, -1);

  char message[] = "hello-world";
  int written = 0;
  while (written < strlen(message)) {
    int bytes = write(fd, message + written, strlen(message) - written);
    ASSERT_NE(bytes, -1);
    written += bytes;
  }

  ASSERT_EQ(fsync(fd), 0);

  lookup_response response = nfs.lookup(ROOT_INO, "file");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  {
    read_response file = nfs.read(response.ino);
    std::string actual_content = std::string(file.content, file.content + file.content_length);
    ASSERT_EQ(actual_content, "hello-world");
  }

  strcpy(message, "-and-bye");

  written = 0;
  while (written < strlen(message)) {
    int bytes = write(fd, message + written, strlen(message) - written);
    ASSERT_NE(bytes, -1);
    written += bytes;
  }

  ASSERT_EQ(close(fd), 0);

  {
    read_response file = nfs.read(response.ino);
    std::string actual_content = std::string(file.content, file.content + file.content_length);
    ASSERT_EQ(actual_content, "hello-world-and-bye");
  }
}

/* This test can be omitted if you don't submit "encoding" bonus. */
TEST_F(FileTest, WriteSpecial) {
  nfs.clear();

  std::string content;
  for (unsigned char c = ' '; c <= 127u; c++) {
    content.append({(char)c});
  }

  std::fstream fs;
  fs.open("file", std::ios::out | std::ios::binary);
  ASSERT_FALSE(fs.fail());

  fs.write(content.c_str(), content.size());
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "file");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, content);
}

TEST_F(FileTest, WriteLong) {
  nfs.clear();

  std::string content(512, 'c');

  std::fstream fs;
  fs.open("file", std::ios::out);
  ASSERT_FALSE(fs.fail());

  fs << content;
  fs.close();
  ASSERT_FALSE(fs.fail());

  lookup_response response = nfs.lookup(ROOT_INO, "file");
  ASSERT_EQ(response.status, 0);
  ASSERT_EQ(response.entry_type, EntryType::FILE);

  read_response file = nfs.read(response.ino);
  std::string actual_content = std::string(file.content, file.content + file.content_length);
  ASSERT_EQ(actual_content, content);
}

TEST_F(FileTest, WriteLonger) {
  nfs.clear();

  std::string content(513, 'c');

  std::fstream fs;
  fs.open("file", std::ios::out);
  ASSERT_FALSE(fs.fail());

  fs << content;
  fs.close();
  ASSERT_TRUE(fs.fail());
}
