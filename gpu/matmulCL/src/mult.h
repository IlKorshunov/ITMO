#define CL_TARGET_OPENCL_VERSION 120
#ifdef __APPLE__
#include <OpenCL/cl.h>
#else
#include <CL/cl.h>
#endif
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

enum class DeviceType { IGPU, DGPU, GPU, CPU, ANY };

template <typename T, cl_int (*ReleaseFunc)(T)>
struct OpenCLRAII {
    T resource;

    OpenCLRAII(T res) : resource(res) {}
    OpenCLRAII(const OpenCLRAII&) = delete;
    OpenCLRAII& operator=(const OpenCLRAII&) = delete;

    OpenCLRAII(OpenCLRAII&& other) noexcept : resource(std::exchange(other.resource, nullptr)) {}
    OpenCLRAII& operator=(OpenCLRAII&& other) noexcept {
        if (this != &other) {
            if (resource) ReleaseFunc(resource);
            resource = std::exchange(other.resource, nullptr);
        }
        return *this;
    }

    ~OpenCLRAII() {
        if (resource) ReleaseFunc(resource);
    }

    operator T() const { return resource; }
    T* get() { return &resource; }
};

using ContextRAII = OpenCLRAII<cl_context, clReleaseContext>;
using ProgramRAII = OpenCLRAII<cl_program, clReleaseProgram>;
using KernelRAII = OpenCLRAII<cl_kernel, clReleaseKernel>;
using QueueRAII = OpenCLRAII<cl_command_queue, clReleaseCommandQueue>;
using EventRAII = OpenCLRAII<cl_event, clReleaseEvent>;
using MemRAII = OpenCLRAII<cl_mem, clReleaseMemObject>;

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

inline void check_error(cl_int e, const char* msg) {
    if (e != CL_SUCCESS) {
        std::cerr << msg << " (code: " << e << ")" << "\n";
        throw std::runtime_error(msg);
    }
}

inline double ns2ms(cl_ulong ns) { return ns * 1e-6; }

inline uint32_t round_up(uint32_t x, uint32_t tile) { return ((x + tile - 1) / tile) * tile; }
inline int my_ceil(int a, int b) { return (a + b - 1) / b; }

template <typename Handle, typename InfoFunc>
std::string getInfoString(Handle handle, InfoFunc infoFunc, cl_uint param) {
    size_t count = 0;
    infoFunc(handle, param, 0, nullptr, &count);
    std::string result(count, '\0');
    infoFunc(handle, param, count, result.data(), nullptr);
    return result;
}

ContextRAII createContext(cl_platform_id platform, cl_device_id device);
QueueRAII createQueue(cl_context context, cl_device_id device);
std::string loadSource(const char* cl_source_path);
ProgramRAII buildProgram(cl_context context, cl_device_id device, const std::string& source, const char* build_opts);
KernelRAII createKernel(cl_program program, const char* kernel_name);

struct DeviceInfo {
    cl_platform_id platform;
    cl_device_id device;
    std::string device_name;
    std::string platform_name;
    DeviceType type;

    DeviceInfo(cl_platform_id platform, cl_device_id device, const std::string& device_name, const std::string& platform_name, DeviceType t)
        : platform(platform), device(device), device_name(device_name), platform_name(platform_name), type(t) {}
};

struct Timings {
    double kernel_ms;
    double total_ms;
    double gflops;
};

struct CommandLineArgs {
    std::string input_file;
    std::string output_file;
    std::string device_type = "all";
    int device_index = 0;
    int realization = -1;
    bool no_omp = false;
    int omp_threads = 16;
    int vec = -1;
    int rpi = -1;
    int tile_size = -1;
};

struct OptimalConstants {
    int TILE_SIZE;
    size_t groups_x, groups_y;
    size_t vec, rpi;
    size_t global_size_x, global_size_y;
    size_t local_size_x, local_size_y;
    uint32_t M_round, N_round, K_round;
};

struct Ocl {
    cl_platform_id platform = nullptr;
    cl_device_id device = nullptr;
    ContextRAII context{nullptr};
    QueueRAII queue{nullptr};
    ProgramRAII program{nullptr};
    KernelRAII kernel{nullptr};
    std::string device_name;
    std::string platform_name;

    Ocl() = default;

    Ocl(cl_platform_id p, cl_device_id d, ContextRAII&& ctx, QueueRAII&& q, ProgramRAII&& prog, KernelRAII&& ker, std::string dn = {}, std::string pn = {})
        : platform(p), device(d), context(std::move(ctx)), queue(std::move(q)), program(std::move(prog)), kernel(std::move(ker)), device_name(std::move(dn)), platform_name(std::move(pn)) {}

    static Ocl build(const DeviceInfo& info, const char* cl_source_path, const char* kernel_name, const char* build_opts) {
        std::string source = loadSource(cl_source_path);
        ContextRAII ctx = createContext(info.platform, info.device);
        QueueRAII q = createQueue(ctx, info.device);
        ProgramRAII prog = buildProgram(ctx, info.device, source, build_opts);
        KernelRAII ker = createKernel(prog, kernel_name);
        return Ocl(info.platform, info.device, std::move(ctx), std::move(q), std::move(prog), std::move(ker), info.device_name, info.platform_name);
    }

    Ocl(const Ocl&) = delete;
    Ocl& operator=(const Ocl&) = delete;
    Ocl(Ocl&&) noexcept = default;
    Ocl& operator=(Ocl&&) noexcept = default;
    ~Ocl() = default;
};

struct MatmulBuffers {
    MemRAII bufA{nullptr}, bufB{nullptr}, bufC{nullptr};

    MatmulBuffers(cl_context context, int M, int N, int K) {
        size_t bytesA = (size_t)M * K * sizeof(float);
        size_t bytesB = (size_t)K * N * sizeof(float);
        size_t bytesC = (size_t)M * N * sizeof(float);

        cl_int err = CL_SUCCESS;
        bufA = MemRAII(clCreateBuffer(context, CL_MEM_READ_ONLY, bytesA, nullptr, &err));
        check_error(err, "clCreateBuffer bufA");
        bufB = MemRAII(clCreateBuffer(context, CL_MEM_READ_ONLY, bytesB, nullptr, &err));
        check_error(err, "clCreateBuffer bufB");
        bufC = MemRAII(clCreateBuffer(context, CL_MEM_WRITE_ONLY, bytesC, nullptr, &err));
        check_error(err, "clCreateBuffer bufC");
    }
};

size_t getMaxGroup(cl_device_id device);
size_t getLocalMemSize(cl_device_id device);
void set_matmul_args(cl_kernel kernel, cl_mem dA, cl_mem dB, cl_mem dC, uint32_t M, uint32_t N, uint32_t K);
Timings run_matmul(const Ocl& ocl, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, const OptimalConstants& constants);

void read_text_input(const std::string& path, uint32_t& N, uint32_t& K, uint32_t& M, std::vector<float>& A, std::vector<float>& B);
void write_text_output(const std::string& path, uint32_t N, uint32_t M, const std::vector<float>& C, int precision = 6);
void read_input(const std::string& path, uint32_t& n, uint32_t& k, uint32_t& m, std::vector<float>& A, std::vector<float>& B);
void write_output(const std::string& path, uint32_t n, uint32_t m, const std::vector<float>& C);

std::vector<cl_platform_id> getPlatforms();
std::vector<cl_device_id> getDevices(cl_platform_id platform);
std::vector<DeviceInfo> getAllDevicesSorted();
DeviceType stringToDeviceType(const std::string& type_str);
DeviceInfo selectDevice(const std::string& device_type, int device_index);

OptimalConstants calculateOptimalConstants(uint32_t M, uint32_t N, uint32_t K, int realization, size_t max_size, size_t mem_size, int vec = -1, int rpi = -1, int tile_size = -1);
std::string generateBuildOpts(const OptimalConstants& constants);
const char* getKernelName(int realization);
CommandLineArgs parse_args(int argc, char** argv);
void padMatrices(const std::vector<float>& A, const std::vector<float>& B, uint32_t M, uint32_t N, uint32_t K, std::vector<float>& A_padded, std::vector<float>& B_padded, std::vector<float>& C_padded,
                 uint32_t& M_padded, uint32_t& N_padded, uint32_t& K_padded);
void print_timings(const Timings& t, const OptimalConstants& c, const std::string& device_name, const std::string& platform_name);
void run_cpu_openmp_realization(const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, uint32_t M, uint32_t N, uint32_t K, const std::string& outPath);
void usage();

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