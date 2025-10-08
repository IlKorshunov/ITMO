#include <fstream>
#include <iomanip>
#include <stdexcept>
#include "mult.h"

template <typename T>
void read_exact(std::ifstream& f, T& dst) {
    f.read(reinterpret_cast<char*>(&dst), sizeof(T));
    if (!f) throw std::runtime_error("read error");
}

template <typename T>
void write_exact(std::ofstream& f, const T& src) {
    f.write(reinterpret_cast<const char*>(&src), sizeof(T));
    if (!f) throw std::runtime_error("Write error");
}

void read_text_input(const std::string& path, uint32_t& N, uint32_t& K, uint32_t& M, std::vector<float>& A, std::vector<float>& B) {
    FileRAII fin(path);
    if (!(fin.get() >> N >> K >> M)) throw std::runtime_error("expected: N K M");

    if (N == 0 || K == 0 || M == 0) throw std::runtime_error("Matrix dimensions must be positive");

    A.resize(static_cast<size_t>(M) * K);
    B.resize(static_cast<size_t>(K) * N);

    for (auto& v : A)
        if (!(fin.get() >> v)) throw std::runtime_error("Read error in A");
    for (auto& v : B)
        if (!(fin.get() >> v)) throw std::runtime_error("Read error in B");
}

void write_text_output(const std::string& path, uint32_t N, uint32_t M, const std::vector<float>& C, int precision) {
    OutputFileRAII fout(path);
    fout.get() << N << " " << M << "\n" << std::fixed << std::setprecision(precision);

    for (size_t i = 0; i < M; ++i) {
        for (size_t j = 0; j < N; ++j) { fout.get() << C[i * N + j] << (j + 1 < N ? ' ' : '\n'); }
    }
}

void read_input(const std::string& path, uint32_t& n, uint32_t& k, uint32_t& m, std::vector<float>& A, std::vector<float>& B) {
    BinaryFileRAII f(path);

    read_exact(f.get(), n);
    read_exact(f.get(), k);
    read_exact(f.get(), m);

    if (n == 0 || k == 0 || m == 0) throw std::runtime_error("Matrix dimensions must be positive");

    A.resize(static_cast<size_t>(m) * k);
    B.resize(static_cast<size_t>(k) * n);

    f.get().read(reinterpret_cast<char*>(A.data()), A.size() * sizeof(float));
    f.get().read(reinterpret_cast<char*>(B.data()), B.size() * sizeof(float));
    if (!f.get()) throw std::runtime_error("Read error in binary matrices");
}

void write_output(const std::string& path, uint32_t n, uint32_t m, const std::vector<float>& C) {
    BinaryOutputFileRAII f(path);
    write_exact(f.get(), n);
    write_exact(f.get(), m);
    f.get().write(reinterpret_cast<const char*>(C.data()), C.size() * sizeof(float));
    if (!f.get()) throw std::runtime_error("Write error in binary matrix C");
}