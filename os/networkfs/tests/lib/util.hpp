#ifndef NETWORKFS_TEST_UTIL_HPP
#define NETWORKFS_TEST_UTIL_HPP

#include <algorithm>
#include <filesystem>
#include <string_view>

namespace fs = std::filesystem;

constexpr ino_t ROOT_INO = 1000;
constexpr std::string_view API_BASE = "/teaching/os/networkfs/v1/";
const fs::path TEST_ROOT = fs::path("/mnt/networkfs-test");

std::set<std::string> list_directory(const fs::path& path);

#endif
