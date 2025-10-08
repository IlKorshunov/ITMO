#include "mult.h"

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

ContextRAII createContext(cl_platform_id platform, cl_device_id device) {
    cl_context_properties props[] = {CL_CONTEXT_PLATFORM, (cl_context_properties)platform, 0};
    cl_int err = CL_SUCCESS;
    cl_context context = clCreateContext(props, 1, &device, nullptr, nullptr, &err);
    check_error(err, "error in create context");
    return ContextRAII(context);
}

QueueRAII createQueue(cl_context context, cl_device_id device) {
    cl_int err = CL_SUCCESS;
    cl_command_queue queue = clCreateCommandQueue(context, device, CL_QUEUE_PROFILING_ENABLE, &err);
    check_error(err, "error in create queue");
    return QueueRAII(queue);
}

size_t getMaxGroup(cl_device_id device) {
    size_t out = 0;
    cl_int err = clGetDeviceInfo(device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(out), &out, nullptr);
    check_error(err, "failed to get max_gr_size");
    return out;
}

size_t getLocalMemSize(cl_device_id device) {
    cl_ulong out = 0;
    cl_int err = clGetDeviceInfo(device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(out), &out, nullptr);
    check_error(err, "failed to get local_mem_size");
    return static_cast<size_t>(out);
}

std::string loadSource(const char* cl_source_path) {
    FileRAII file(cl_source_path);
    std::ostringstream source;
    source << file.get().rdbuf();
    return source.str();
}

ProgramRAII buildProgram(cl_context context, cl_device_id device, const std::string& source, const char* build_opts) {
    cl_int err = CL_SUCCESS;
    const char* src = source.c_str();
    size_t size = source.size();
    cl_program program = clCreateProgramWithSource(context, 1, &src, &size, &err);
    check_error(err, "error in create program");

    ProgramRAII program_raii(program);

    err = clBuildProgram(program, 1, &device, build_opts, nullptr, nullptr);
    if (err != CL_SUCCESS) {
        size_t log_size = 0;
        clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, nullptr, &log_size);
        std::string build_log(log_size, '\0');
        clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, log_size, build_log.data(), nullptr);
        std::cerr << "Build log: " << build_log << "\n";
        check_error(err, "error in build program");
    }

    return program_raii;
}

KernelRAII createKernel(cl_program program, const char* kernel_name) {
    cl_int err = CL_SUCCESS;
    cl_kernel kernel = clCreateKernel(program, kernel_name, &err);
    check_error(err, "error in create kernel");
    return KernelRAII(kernel);
}

void set_matmul_args(cl_kernel kernel, cl_mem dA, cl_mem dB, cl_mem dC, uint32_t M, uint32_t N, uint32_t K) {
    check_error(clSetKernelArg(kernel, 0, sizeof(cl_mem), &dA), "error in set arg0 (A)");
    check_error(clSetKernelArg(kernel, 1, sizeof(cl_mem), &dB), "error in set arg1 (B)");
    check_error(clSetKernelArg(kernel, 2, sizeof(cl_mem), &dC), "error in set arg2 (C)");
    check_error(clSetKernelArg(kernel, 3, sizeof(int), &M), "error in set arg3 (M)");
    check_error(clSetKernelArg(kernel, 4, sizeof(int), &N), "error in set arg4 (N)");
    check_error(clSetKernelArg(kernel, 5, sizeof(int), &K), "error in set arg5 (K)");
}

struct KernelExecution {
    EventRAII evWriteA, evWriteB, evKernel, evRead;

    KernelExecution(cl_command_queue queue, const MatmulBuffers& buffers, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, cl_kernel kernel,
                    const OptimalConstants& constants)
        : evWriteA(nullptr), evWriteB(nullptr), evKernel(nullptr), evRead(nullptr) {

        size_t bytesA = (size_t)constants.M_round * constants.K_round * sizeof(float);
        size_t bytesB = (size_t)constants.K_round * constants.N_round * sizeof(float);
        size_t bytesC = (size_t)constants.M_round * constants.N_round * sizeof(float);

        check_error(clEnqueueWriteBuffer(queue, buffers.bufA, CL_FALSE, 0, bytesA, A.data(), 0, nullptr, evWriteA.get()), "error in write A");
        check_error(clEnqueueWriteBuffer(queue, buffers.bufB, CL_FALSE, 0, bytesB, B.data(), 0, nullptr, evWriteB.get()), "error in write B");

        set_matmul_args(kernel, buffers.bufA, buffers.bufB, buffers.bufC, constants.M_round, constants.N_round, constants.K_round);

        size_t local[2] = {constants.local_size_x, constants.local_size_y};
        size_t global[2] = {constants.global_size_x, constants.global_size_y};

        const cl_event deps[] = {evWriteA, evWriteB};
        check_error(clEnqueueNDRangeKernel(queue, kernel, 2, nullptr, global, local, 2, deps, evKernel.get()), "error in enqueue kernel");
        check_error(clEnqueueReadBuffer(queue, buffers.bufC, CL_FALSE, 0, bytesC, C.data(), 1, evKernel.get(), evRead.get()), "error in read C");
        //check_error(clWaitForEvents(1, evRead.get()), "error in wait read");
    }

    std::pair<cl_ulong, cl_ulong> getProfilingData(const EventRAII& event) {
        cl_ulong start = 0, end = 0;
        check_error(clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(start), &start, nullptr), "error in prof start");
        check_error(clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end), &end, nullptr), "error in prof end");
        return {start, end};
    }
};

Timings run_matmul(const Ocl& ocl, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, const OptimalConstants& constants) {
    if ((constants.global_size_x % constants.local_size_x) || (constants.global_size_y % constants.local_size_y)) { throw std::runtime_error("global not multiple of local"); }
    cl_command_queue q = ocl.queue;
    MatmulBuffers buffers(ocl.context, constants.M_round, constants.N_round, constants.K_round);
    KernelExecution execution(q, buffers, A, B, C, ocl.kernel, constants);

    cl_event events[] = {execution.evWriteA, execution.evWriteB, execution.evKernel, execution.evRead};    check_error(clWaitForEvents(4, events), "error in profiling");
    auto [start_a, end_a] = execution.getProfilingData(execution.evWriteA);
    auto [start_b, end_b] = execution.getProfilingData(execution.evWriteB);
    auto [start_kernel, end_kernel] = execution.getProfilingData(execution.evKernel);
    auto [start_read, end_read] = execution.getProfilingData(execution.evRead);

    Timings t{};
    t.kernel_ms = ns2ms(end_kernel - start_kernel);
    cl_ulong begin = std::min(start_a, start_b);
    t.total_ms = ns2ms(end_read - begin);

    double kernel_sec = (end_kernel - start_kernel) * 1e-9;
    double ops = 2.0 * constants.M_round * constants.N_round * constants.K_round;
    t.gflops = ops / (kernel_sec * 1e9);

    return t;
}