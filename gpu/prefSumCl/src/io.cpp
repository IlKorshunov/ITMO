#include <cstdint>
#include <fstream>
#include <stdexcept>
#include <string>
#include <vector>
#include "sum.h"

template <typename T>
void read_exact(std::ifstream& f, T& dst) {
    f.read(reinterpret_cast<char*>(&dst), sizeof(T));
    if (!f) throw std::runtime_error("Read error");
}

template <typename T>
void write_exact(std::ofstream& f, const T& src) {
    f.write(reinterpret_cast<const char*>(&src), sizeof(T));
    if (!f) throw std::runtime_error("Write error");
}

void read_text_input(const std::string& path, std::vector<uint32_t>& data) {
    FileRAII fin(path);
    uint32_t n = 0;
    if (!(fin.get() >> n)) throw std::runtime_error("Couldn't read n");
    if (n <= 0) throw std::runtime_error("Array size must be positive");

    data.resize(n);
    for (uint32_t i = 0; i < n; ++i) {
        if (!(fin.get() >> data[i])) { throw std::runtime_error("Read error at element " + std::to_string(i)); }
    }
}

void write_text_output(const std::string& path, const std::vector<uint32_t>& data) {
    OutputFileRAII fout(path);
    fout.get() << data.size() << "\n";
    for (size_t i = 0; i < data.size(); ++i) { fout.get() << data[i] << (i + 1 < data.size() ? ' ' : '\n'); }
}

void read_input(const std::string& path, std::vector<uint32_t>& data) {
    BinaryFileRAII f(path);

    uint32_t n = 0;
    read_exact(f.get(), n);
    if (n <= 0) throw std::runtime_error("Array size must be positive");

    data.resize(n);
    f.get().read(reinterpret_cast<char*>(data.data()), n * sizeof(uint32_t));
    if (!f.get()) throw std::runtime_error("Read error in binary array");
}

void write_output(const std::string& path, const std::vector<uint32_t>& data) {
    BinaryOutputFileRAII f(path);

    uint32_t n = static_cast<uint32_t>(data.size());
    write_exact(f.get(), n);

    f.get().write(reinterpret_cast<const char*>(data.data()), n * sizeof(uint32_t));
    if (!f.get()) throw std::runtime_error("Write error in binary array");
}
