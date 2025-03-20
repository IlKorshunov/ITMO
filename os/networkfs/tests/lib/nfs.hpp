#ifndef NETWORKFS_TEST_NFS_HPP
#define NETWORKFS_TEST_NFS_HPP

#include <string>
#include <httplib.h>

#include "util.hpp"

enum class EntryType {
  DIRECTORY = 4,
  FILE = 8
};

struct token_response {
  uint64_t status;
  char token[36];
};

struct list_response {
  uint64_t status;
  size_t entries_count;
  struct entry {
    EntryType entry_type;
    ino_t ino;
    char name[256];
  } entries[16];
};

struct create_response {
  uint64_t status;
  ino_t ino;
};

struct read_response {
  uint64_t status;
  uint64_t content_length;
  char content[512];
};

struct empty_response {
  uint64_t status;
};

struct lookup_response {
  uint64_t status;
  EntryType entry_type;
  ino_t ino;
};

class NfsBucket {
private:
  bool mounted = false;
  std::string token_;
  httplib::Client client;

  std::string call_api(const std::string&, const httplib::Params& = {}, size_t = 0);
public:
  NfsBucket();
  
  // I'm unsure of whether client can be copied, so here we go.
  NfsBucket(const NfsBucket&) = delete;
  NfsBucket(NfsBucket&&) = delete;
  NfsBucket& operator=(const NfsBucket&) = delete;
  NfsBucket& operator=(NfsBucket&&) = delete;

  const std::string token() const;

  void initialize();
  void unmount(bool);

  ~NfsBucket();

  struct token_response issue();
  struct list_response list(ino_t);
  struct create_response create(ino_t, const std::string&, EntryType);
  struct read_response read(ino_t);
  struct empty_response write(ino_t, const std::string&);
  struct empty_response link(ino_t, ino_t, const std::string&);
  struct empty_response unlink(ino_t, const std::string&);
  struct empty_response rmdir(ino_t, const std::string&);
  struct lookup_response lookup(ino_t, const std::string&);

  void clear(ino_t = ROOT_INO); /* Empties whole filesystem */
};

constexpr size_t MAX_ATTEMPTS = 3;
constexpr size_t REQUEST_DELAY = 1'000;

#endif
