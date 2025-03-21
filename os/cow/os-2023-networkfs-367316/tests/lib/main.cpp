#include <filesystem>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

#include <gtest/gtest.h>
#include <fcntl.h>

#include "util.hpp"

namespace fs = std::filesystem;

class Environment : public ::testing::Environment {
private:
  bool unload_module = true;
  bool delete_mountpoint = false;

public:
  ~Environment() override {}

  void SetUp() override {
    fs::path module_path("networkfs.ko");

    if (!fs::exists(module_path)) {
      throw std::runtime_error("Module networkfs is not built");
    }

    int fd = open(module_path.c_str(), O_RDONLY);
    if (fd == -1) {
      throw std::runtime_error(std::string("Module networkfs is not accessible: ") + strerror(errno));
    }

    if (syscall(SYS_finit_module, fd, "", 0)) {
      switch (errno) {
        case EPERM:
          throw std::runtime_error("Can not load module: Permission denied. Try re-running tests as root.");
        case EEXIST:
          std::cerr << "warning: using already loaded module" << std::endl;
          unload_module = false;
          break;
        default:
          throw std::runtime_error(std::string("Can not load module: ") + strerror(errno));
      }
    }

    if (close(fd)) {
      std::cerr << "warning: could not close module file" << std::endl;
    }

    if (!fs::exists(TEST_ROOT)) {
      fs::create_directories(TEST_ROOT);
      delete_mountpoint = true;
    }
  }

  void TearDown() override {
    if (delete_mountpoint) {
      fs::remove(TEST_ROOT);
    }

    if (unload_module) {
      if (syscall(SYS_delete_module, "networkfs", 0)) {
        throw std::runtime_error(std::string("Can not unload module:") + strerror(errno));
      }
    }
  }
};


int main(int argc, char **argv) {
  // gtest now owns this environment, no need to delete
  testing::AddGlobalTestEnvironment(new Environment);
  testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
