#pragma once

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
    explicit OpenCLRAII(T res = nullptr) : resource(res) {}
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
    explicit FileRAII(const std::string& path) : file(path) {
        if (!file) throw std::runtime_error("Cannot open file: " + path);
    }
    ~FileRAII() {
        if (file.is_open()) file.close();
    }
    std::ifstream& get() { return file; }
    operator std::ifstream&() { return file; }
};

struct OutputFileRAII {
    std::ofstream file;
    explicit OutputFileRAII(const std::string& path) : file(path) {
        if (!file) throw std::runtime_error("Cannot open output file: " + path);
    }
    ~OutputFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ofstream& get() { return file; }
    operator std::ofstream&() { return file; }
};

struct BinaryFileRAII {
    std::ifstream file;
    explicit BinaryFileRAII(const std::string& path) : file(path, std::ios::in | std::ios::binary) {
        if (!file) throw std::runtime_error("Cannot open binary file: " + path);
    }
    ~BinaryFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ifstream& get() { return file; }
};

struct BinaryOutputFileRAII {
    std::ofstream file;
    explicit BinaryOutputFileRAII(const std::string& path) : file(path, std::ios::out | std::ios::binary) {
        if (!file) throw std::runtime_error("Cannot open binary output file: " + path);
    }
    ~BinaryOutputFileRAII() {
        if (file.is_open()) file.close();
    }
    std::ofstream& get() { return file; }
};

inline void check_error(cl_int e, const char* msg) {
    if (e != CL_SUCCESS) {
        std::cerr << msg << " (code: " << e << ")\n";
        throw std::runtime_error(msg);
    }
}
inline double ns2ms(cl_ulong ns) { return ns * 1e-6; }

template <typename Handle, typename InfoFunc>
std::string getInfoString(Handle handle, InfoFunc infoFunc, cl_uint param) {
    size_t count = 0;
    infoFunc(handle, param, 0, nullptr, &count);
    std::string result(count, '\0');
    infoFunc(handle, param, count, result.data(), nullptr);
    return result;
}

struct DeviceInfo {
    cl_platform_id platform{};
    cl_device_id device{};
    std::string device_name;
    std::string platform_name;
    DeviceType type{DeviceType::ANY};
    DeviceInfo() = default;
    DeviceInfo(cl_platform_id p, cl_device_id d, std::string dn, std::string pn, DeviceType t) : platform(p), device(d), device_name(std::move(dn)), platform_name(std::move(pn)), type(t) {}
};

ContextRAII createContext(cl_platform_id platform, cl_device_id device);
QueueRAII createQueue(cl_platform_id platform, cl_context context, cl_device_id device);
std::string loadSource(const char* cl_source_path);
ProgramRAII buildProgram(cl_context context, cl_device_id device, const std::string& source, const char* build_opts);
KernelRAII createKernel(cl_program program, const char* kernel_name);

std::vector<cl_platform_id> getPlatforms();
std::vector<cl_device_id> getDevices(cl_platform_id platform);
std::vector<DeviceInfo> getAllDevicesSorted();
DeviceType stringToDeviceType(const std::string& type_str);
DeviceInfo selectDevice(const std::string& device_type, int device_index);

template <typename T>
void read_exact(std::ifstream& f, T& dst);
template <typename T>
void write_exact(std::ofstream& f, const T& src);
void read_text_input(const std::string& path, std::vector<uint32_t>& data);
void write_text_output(const std::string& path, const std::vector<uint32_t>& data);
void read_input(const std::string& path, std::vector<uint32_t>& data);
void write_output(const std::string& path, const std::vector<uint32_t>& data);

struct Timings {
    double kernel_ms;
    double total_ms;
};

struct CommandLineArgs {
    std::string input_file;
    std::string output_file;
    std::string device_type = "all";
    int device_index = 0;
    int tile_size = 64;
    int vec = 4;
};

struct OptimalConstants {
    int TILE_SIZE;
    int VEC;
};

OptimalConstants calculateOptimalConstants(uint32_t N, const CommandLineArgs& args);
std::string generateBuildOpts(const OptimalConstants& constants);
CommandLineArgs parse_args(int argc, char** argv);

void usage();

inline size_t round_up_mul(size_t x, size_t mul) { return (x + mul - 1) / mul * mul; }
inline size_t ceil_div(size_t x, size_t y) { return (x + y - 1) / y; }

void fill_zero(cl_command_queue q, MemRAII& buf, size_t offset_bytes, size_t size_bytes);
void pad_block(cl_command_queue q, MemRAII& buf, size_t logical_len, size_t padded_len);

struct Kernels {
    KernelRAII block_scan{nullptr};
    KernelRAII add_offsets{nullptr};
    KernelRAII brent_kung{nullptr};
    Kernels() = default;
    Kernels(const Kernels&) = delete;
    Kernels& operator=(const Kernels&) = delete;
    Kernels(Kernels&&) noexcept = default;
    Kernels& operator=(Kernels&&) noexcept = default;
};

struct Level {
    MemRAII data;
    size_t logical_len = 0;
    size_t padded_len = 0;
    size_t blocks = 0;

    Level() = default;
    Level(MemRAII&& buf, size_t logical_len_, size_t TILE) : data(std::move(buf)), logical_len(logical_len_) {
        padded_len = round_up_mul(logical_len, TILE);
        blocks = ceil_div(logical_len, TILE);
    }
};

Kernels make_prefix_kernels(cl_program program);

void enqueue_block_scan(cl_command_queue q, Kernels& k, Level& lvl, cl_mem block_sums, const OptimalConstants& c);
void enqueue_add_offsets(cl_command_queue q, Kernels& k, Level& lvl, cl_mem block_prefix, const OptimalConstants& c);
void enqueue_brent_kung(cl_command_queue q, Kernels& k, cl_mem data, const OptimalConstants& c);

int build_upper_levels(cl_command_queue q, cl_context ctx, Kernels& k, std::vector<Level>& levels, const OptimalConstants& constants);
void global_pref_sum(cl_command_queue q, cl_context ctx, Kernels& k, MemRAII& src, size_t logical_len, const OptimalConstants& constants, std::vector<Level>& levels);

struct Ocl {
    cl_platform_id platform = nullptr;
    cl_device_id device = nullptr;
    ContextRAII context{nullptr};
    QueueRAII queue{nullptr};
    ProgramRAII program{nullptr};
    Kernels kernels{};
    std::string device_name;
    std::string platform_name;

    Ocl() = default;

    Ocl(cl_platform_id p, cl_device_id d, ContextRAII&& ctx, QueueRAII&& q, ProgramRAII&& prog, Kernels&& ks, std::string dn = {}, std::string pn = {})
        : platform(p), device(d), context(std::move(ctx)), queue(std::move(q)), program(std::move(prog)), kernels(std::move(ks)), device_name(std::move(dn)), platform_name(std::move(pn)) {}

    static Ocl build(const DeviceInfo& info, const char* cl_source_path, const char* build_opts) {
        std::string source = loadSource(cl_source_path);
        ContextRAII ctx = createContext(info.platform, info.device);
        QueueRAII q = createQueue(info.platform, ctx, info.device);
        ProgramRAII prog = buildProgram(ctx, info.device, source, build_opts);
        Kernels ks = make_prefix_kernels(prog);
        return Ocl(info.platform, info.device, std::move(ctx), std::move(q), std::move(prog), std::move(ks), info.device_name, info.platform_name);
    }

    Ocl(const Ocl&) = delete;
    Ocl& operator=(const Ocl&) = delete;
    Ocl(Ocl&&) noexcept = default;
    Ocl& operator=(Ocl&&) noexcept = default;
    ~Ocl() = default;
};

Timings run_prefix_sum(const Ocl& ocl, const std::vector<uint32_t>& input, std::vector<uint32_t>& output, const OptimalConstants& constants);
void print_timings(const Timings& t, const OptimalConstants& constants, const std::string& device_name, const std::string& platform_name);
extern std::vector<std::string> g_kernel_log;
