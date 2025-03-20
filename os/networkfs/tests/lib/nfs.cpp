#include <chrono>
#include <filesystem>
#include <sys/mount.h>
#include <thread>

#include "nfs.hpp"
#include "util.hpp"

namespace fs = std::filesystem;

NfsBucket::NfsBucket() : client("nerc.itmo.ru", 80) {}

void NfsBucket::initialize() {
  auto response = issue();
  this->token_ = std::string(response.token, response.token + sizeof(response.token));

  if (mount(this->token_.data(), TEST_ROOT.c_str(), "networkfs", 0, "")) {
    throw std::runtime_error(std::string("Filesystem can not be mounted: ") + strerror(errno));
  }

  this->mounted = true;
}

const std::string NfsBucket::token() const {
  return token_;
}

void NfsBucket::unmount(bool do_throw) {
  this->mounted = false;

  for (int i = 0; i < 5; i++) {
    if (!umount(TEST_ROOT.c_str())) {
      return;
    }
    sleep(1);
  }

  if (do_throw) {
    throw std::runtime_error(std::string("Filesystem can not be unmounted: ") + strerror(errno));
  } else {
    std::cerr << "error: Filesystem can not be unmounted: " << strerror(errno) << std::endl;
  }
}

NfsBucket::~NfsBucket() {
  if (this->mounted) {
    std::cerr << "warning: you shouldn't rely on filesystem unmounting in destructor, use NfsBucket::unmount" << std::endl;
    unmount(false);
  }
}

void NfsBucket::clear(ino_t ino) {
  auto response = list(ino);
  if (response.status == 1) return;
  if (response.status != 0) throw std::runtime_error("Unexpected status " + std::to_string(response.status));

  for (int i = 0; i < response.entries_count; i++) {
    if (response.entries[i].entry_type == EntryType::FILE) {
      if (uint64_t status = unlink(ino, response.entries[i].name).status) {
        throw std::runtime_error("Unexpected status " + std::to_string(status));
      }
    } else {
      clear(response.entries[i].ino);
      if (uint64_t status = rmdir(ino, response.entries[i].name).status) {
        throw std::runtime_error("Unexpected status " + std::to_string(status));
      }
    }
  }
}

std::string NfsBucket::call_api(const std::string& uri, const httplib::Params& params, size_t attempts) {
  std::string full_uri = std::string(API_BASE);

  if (!uri.starts_with("token")) {
    full_uri += token();
    full_uri += "/";
  }

  full_uri += uri;

  auto req = client.Get(full_uri, params, {});

  if (!req) {
    if (attempts == MAX_ATTEMPTS) {
      throw std::runtime_error("Request failed: " + to_string(req.error()));
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(REQUEST_DELAY));
    return call_api(uri, params, attempts + 1);
  }

  if (req->status != 200) {
    throw std::runtime_error("Request failed with status code " + std::to_string(req->status));
  }

  return req->body;
}

template<typename T> T convert(const std::string& from) {
  T value;
  memcpy(&value, from.data(), from.size());
  return value;
}

struct token_response NfsBucket::issue() {
  return convert<token_response>(call_api("token/issue"));
}

struct list_response NfsBucket::list(ino_t inode) {
  return convert<list_response>(call_api("fs/list", {{"inode", std::to_string(inode)}}));
}

struct create_response NfsBucket::create(ino_t parent, const std::string& name, EntryType type) {
  return convert<create_response>(
    call_api(
      "fs/create",
      {
        {"parent", std::to_string(parent)},
        {"name", name},
        {"type", type == EntryType::FILE ? "file" : "directory"}
      }
    )
  );
}

struct read_response NfsBucket::read(ino_t inode) {
  return convert<read_response>(call_api("fs/read", {{"inode", std::to_string(inode)}}));
}

struct empty_response NfsBucket::write(ino_t inode, const std::string& content) {
  return convert<empty_response>(
    call_api(
      "fs/write",
      {
        {"inode", std::to_string(inode)},
        {"content", content}
      }
    )
  );
}

struct empty_response NfsBucket::link(ino_t source, ino_t parent, const std::string& name) {
  return convert<empty_response>(
    call_api(
      "fs/link",
      {
        {"source", std::to_string(source)},
        {"parent", std::to_string(parent)},
        {"name", name}
      }
    )
  );
}
struct empty_response NfsBucket::unlink(ino_t parent, const std::string& name) {
  return convert<empty_response>(
    call_api(
      "fs/unlink",
      {
        {"parent", std::to_string(parent)},
        {"name", name}
      }
    )
  );
}

struct empty_response NfsBucket::rmdir(ino_t parent, const std::string& name) {
  return convert<empty_response>(
    call_api(
      "fs/rmdir",
      {
        {"parent", std::to_string(parent)},
        {"name", name}
      }
    )
  );
}

struct lookup_response NfsBucket::lookup(ino_t parent, const std::string& name) {
  return convert<lookup_response>(
    call_api(
      "fs/lookup",
      {
        {"parent", std::to_string(parent)},
        {"name", name}
      }
    )
  );
}
