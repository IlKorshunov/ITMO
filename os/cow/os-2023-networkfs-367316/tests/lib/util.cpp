#include <algorithm>
#include <filesystem>
#include <set>

namespace fs = std::filesystem;

std::set<std::string> list_directory(const fs::path& path) {
  std::set<std::string> result;

  std::transform(
    fs::directory_iterator(path), fs::directory_iterator(),
    std::inserter(result, result.begin()),
    [](const fs::directory_entry& entry) { return entry.path().filename(); }
  );

  return result;
}
