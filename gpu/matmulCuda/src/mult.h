#pragma once
#include <cuda_runtime.h>
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
#include <omp.h>

enum class DeviceType { IGPU, DGPU, GPU, CPU, ANY };

struct OptimalConstants {
    int TILE_SIZE;
    size_t groups_x, groups_y;
    size_t vec, rpi;
    size_t global_size_x, global_size_y;
    size_t local_size_x, local_size_y;
    uint32_t M_round, N_round, K_round;
    dim3 block, grid;
};

size_t getMaxGroup(int device);
size_t getLocalMemSize(int device);
void launch_matmul_tile(const float* A, const float* B, float* C, int M, int N, int K, dim3 grid, dim3 block, int TILE_SIZE);
void launch_matmul_tiled_vecN(const float* A, const float* B, float* C, int M, int N, int K, dim3 grid, dim3 block, const OptimalConstants& constants);
void usage();

struct FileRAII {
    std::ifstream file;
    FileRAII(const std::string& path) : file(path) {
        if (!file) throw std::runtime_error("Cannot open file: " + path);
    }
    ~FileRAII() {
        if (file.is_open()) file.close();
    }
    std::ifstream& get() { return file; }
    operator std::ifstream&() { return file; }
};

struct BinaryFileRAII {
    std::ifstream file;
    BinaryFileRAII(const std::string& path) : file(path, std::ios::binary) {
        if (!file) throw std::runtime_error("Cannot open file: " + path);
    }
    ~BinaryFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ifstream& get() { return file; }
    operator std::ifstream&() { return file; }
};

struct OutputFileRAII {
    std::ofstream file;
    OutputFileRAII(const std::string& path) : file(path) {
        if (!file) throw std::runtime_error("Cannot open output file: " + path);
    }
    ~OutputFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ofstream& get() { return file; }
    operator std::ofstream&() { return file; }
};

struct BinaryOutputFileRAII {
    std::ofstream file;
    BinaryOutputFileRAII(const std::string& path) : file(path, std::ios::binary) {
        if (!file) throw std::runtime_error("Cannot open output file: " + path);
    }
    ~BinaryOutputFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ofstream& get() { return file; }
    operator std::ofstream&() { return file; }
};

struct CommandLineArgs {
    std::string input_file;
    std::string output_file;
    std::string device_type = "all";
    int device_index = 0;
    int realization = -1;
    bool no_omp = false;
    int omp_threads = 16;
};

struct PaddedMatrices {
    std::vector<float> A_padded, B_padded, C_padded;
    int M_padded = 0, N_padded = 0, K_padded = 0;
};

struct Timings {
    double kernel_ms;
    double total_ms;
    double gflops;
};

struct CudaEventRaii {
    cudaEvent_t ev{};
    CudaEventRaii() { cudaEventCreate(&ev); }
    ~CudaEventRaii() { cudaEventDestroy(ev); }
    operator cudaEvent_t() const { return ev; }
};

struct DeviceInfo {
    int device_index = 0;
    std::string device_name;
    int driverVersion;
    DeviceType type = DeviceType::GPU;
};

void read_text_input(const std::string& path, uint32_t& N, uint32_t& K, uint32_t& M, std::vector<float>& A, std::vector<float>& B);
void write_text_output(const std::string& path, uint32_t N, uint32_t M, const std::vector<float>& C, int precision = 6);
void read_input(const std::string& path, uint32_t& n, uint32_t& k, uint32_t& m, std::vector<float>& A, std::vector<float>& B);
void write_output(const std::string& path, uint32_t n, uint32_t m, const std::vector<float>& C);

OptimalConstants calculateOptimalConstants(uint32_t M, uint32_t N, uint32_t K, int realization, size_t max_size, size_t mem_size);
CommandLineArgs parse_args(int argc, char** argv);

PaddedMatrices padMatrices(const std::vector<float>& A, const std::vector<float>& B, int M, int N, int K, bool enable_padding, int tile_size);
void extractResult(const PaddedMatrices& padded, std::vector<float>& C, int M, int N);
void print_timings(const Timings& t, OptimalConstants& constants, const DeviceInfo& dev);
void run_cpu_openmp_realization(const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, uint32_t M, uint32_t N, uint32_t K, const std::string& outPath);

DeviceInfo selectDevice(const std::string& device_type, int device_index);

Timings run_matmul(const DeviceInfo& dev, const CommandLineArgs& args, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, OptimalConstants& constants);

static inline void cuda_check(cudaError_t e, const char* where) {
    if (e != cudaSuccess) {
        std::cerr << where << " (code: " << static_cast<int>(e) << "): " << cudaGetErrorString(e) << "\n";
        throw std::runtime_error(where);
    }
}

inline double ns2ms(unsigned long long ns) { return ns * 1e-6; }

struct MatmulBuffers {
    float* dA{nullptr};
    float* dB{nullptr};
    float* dC{nullptr};
    size_t bytesA{0}, bytesB{0}, bytesC{0};

    MatmulBuffers(int M, int N, int K) {
        bytesA = (size_t)M * K * sizeof(float);
        bytesB = (size_t)K * N * sizeof(float);
        bytesC = (size_t)M * N * sizeof(float);

        cuda_check(cudaMalloc(&dA, bytesA), "cudaMalloc dA");
        cuda_check(cudaMalloc(&dB, bytesB), "cudaMalloc dB");
        cuda_check(cudaMalloc(&dC, bytesC), "cudaMalloc dC");
    }

    ~MatmulBuffers() {
        if (dA) cudaFree(dA);
        if (dB) cudaFree(dB);
        if (dC) cudaFree(dC);
    }
};

inline uint32_t round_up(uint32_t x, uint32_t tile) { return ((x + tile - 1) / tile) * tile; }

inline int my_ceil(int a, int b) { return (a + b - 1) / b; }

template <typename T>
void MulMatrixBlock(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template <typename T>
void MulMatrixBlockSIMD(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template <typename T>
void MulMatrixOpenMp(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template <typename T>
void MulMatrixOpenMpSIMD(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template <typename T>
void MulMatrixBaseline(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K);
template <typename T>
void MulMatrix(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K);