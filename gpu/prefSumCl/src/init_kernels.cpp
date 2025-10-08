#include <iostream>
#include <sstream>
#include <vector>
#include "sum.h"

ContextRAII createContext(cl_platform_id platform, cl_device_id device) {
    cl_context_properties props[] = {CL_CONTEXT_PLATFORM, (cl_context_properties)platform, 0};
    cl_int err = CL_SUCCESS;
    cl_context ctx = clCreateContext(props, 1, &device, nullptr, nullptr, &err);
    check_error(err, "error in create context");
    return ContextRAII(ctx);
}

QueueRAII createQueue(cl_platform_id platform, cl_context context, cl_device_id device) {
    cl_int err = CL_SUCCESS;

    // std::string plat = getInfoString(platform, clGetPlatformInfo, CL_PLATFORM_NAME);
    // std::string dev  = getInfoString(device, clGetDeviceInfo, CL_DEVICE_NAME);
    // std::cerr << "Platform: " << plat << " Device: " << dev << std::endl;

    cl_command_queue q = clCreateCommandQueue(context, device, CL_QUEUE_PROFILING_ENABLE, &err);
    check_error(err, "error in create queue");
    return QueueRAII(q);
}

std::string loadSource(const char* cl_source_path) {
    FileRAII file(cl_source_path);
    std::ostringstream ss;
    ss << file.get().rdbuf();
    return ss.str();
}

ProgramRAII buildProgram(cl_context context, cl_device_id device, const std::string& source, const char* build_opts) {
    cl_int err = CL_SUCCESS;
    const char* src_ptr = source.c_str();
    size_t src_sz = source.size();
    cl_program prog = clCreateProgramWithSource(context, 1, &src_ptr, &src_sz, &err);
    check_error(err, "error in create program");
    ProgramRAII prog_raii(prog);

    err = clBuildProgram(prog, 1, &device, build_opts, nullptr, nullptr);
    if (err != CL_SUCCESS) {
        size_t log_size = 0;
        clGetProgramBuildInfo(prog, device, CL_PROGRAM_BUILD_LOG, 0, nullptr, &log_size);
        std::string build_log(log_size, '\0');
        clGetProgramBuildInfo(prog, device, CL_PROGRAM_BUILD_LOG, log_size, build_log.data(), nullptr);
        std::cerr << "Build log:\n" << build_log << "\n";
        check_error(err, "error in build program");
    }
    return prog_raii;
}

KernelRAII createKernel(cl_program program, const char* kernel_name) {
    cl_int err = CL_SUCCESS;
    cl_kernel k = clCreateKernel(program, kernel_name, &err);
    if (err != CL_SUCCESS) { std::cerr << "Failed to create kernel '" << kernel_name << "', error code: " << err << std::endl; }
    check_error(err, "error in create kernel");
    return KernelRAII(k);
}

Kernels make_prefix_kernels(cl_program program) {
    Kernels k{};
    k.block_scan = createKernel(program, "block_scan");
    k.add_offsets = createKernel(program, "add_offsets");
    k.brent_kung = createKernel(program, "brent_kung");
    return k;
}

std::vector<std::string> g_kernel_log;


inline void print_kernel_info(const char* name, size_t group_size, size_t groups_count, size_t elems_per_group, size_t global_size, const EventRAII& /*ev*/) {
    std::ostringstream ss;
    ss << "kernel=" << name << " group_size=" << group_size << " groups_count=" << groups_count << " elems_per_group=" << elems_per_group << " global=" << global_size;
    g_kernel_log.push_back(ss.str());
}

void enqueue_block_scan(cl_command_queue q, Kernels& k, Level& lvl, cl_mem block_sums, const OptimalConstants& c) {
    check_error(clSetKernelArg(k.block_scan, 0, sizeof(cl_mem), &lvl.data), "set arg block_scan src");
    check_error(clSetKernelArg(k.block_scan, 1, sizeof(cl_mem), &lvl.data), "set arg block_scan dst");
    check_error(clSetKernelArg(k.block_scan, 2, sizeof(cl_mem), &block_sums), "set arg block_scan block_sums");

    size_t global_size = lvl.padded_len;
    size_t group_size = c.TILE_SIZE;
    size_t elems_per_grp = c.TILE_SIZE;
    size_t groups_count = lvl.blocks;

    EventRAII ev;
    check_error(clEnqueueNDRangeKernel(q, k.block_scan, 1, nullptr, &global_size, &group_size, 0, nullptr, ev.get()), "enqueue block_scan");
    print_kernel_info("block_scan", group_size, groups_count, elems_per_grp, global_size, ev);
}

void enqueue_add_offsets(cl_command_queue q, Kernels& k, Level& lvl, cl_mem block_prefix, const OptimalConstants& c) {
    check_error(clSetKernelArg(k.add_offsets, 0, sizeof(cl_mem), &lvl.data), "set arg add_offsets dst");
    check_error(clSetKernelArg(k.add_offsets, 1, sizeof(cl_mem), &block_prefix), "set arg add_offsets prefix");
    const cl_uint n_u = static_cast<cl_uint>(lvl.logical_len);
    check_error(clSetKernelArg(k.add_offsets, 2, sizeof(cl_uint), &n_u), "set arg add_offsets n");

    size_t global_size = lvl.padded_len / c.VEC;
    size_t group_size = c.TILE_SIZE / c.VEC;
    size_t elems_per_grp = c.TILE_SIZE;
    size_t groups_count = lvl.blocks;

    EventRAII ev;
    check_error(clEnqueueNDRangeKernel(q, k.add_offsets, 1, nullptr, &global_size, &group_size, 0, nullptr, ev.get()), "enqueue add_offsets");
    print_kernel_info("add_offsets", group_size, groups_count, elems_per_grp, global_size, ev);
}



void enqueue_brent_kung(cl_command_queue q, Kernels& k, cl_mem data, const OptimalConstants& c) {
    check_error(clSetKernelArg(k.brent_kung, 0, sizeof(cl_mem), &data), "set arg brent_kung data");

    size_t group_size = c.TILE_SIZE;
    size_t elems_per_group = c.TILE_SIZE;
    size_t global_size = c.TILE_SIZE;
    size_t groups_count = 1;

    EventRAII ev;
    check_error(clEnqueueNDRangeKernel(q, k.brent_kung, 1, nullptr, &global_size, &group_size, 0, nullptr, ev.get()), "enqueue brent_kung");
    print_kernel_info("brent_kung", group_size, groups_count, elems_per_group, global_size, ev);
}

struct PrefixSumExecution {
    MemRAII bufSrc{nullptr};
    EventRAII evWrite{nullptr}, evStart{nullptr}, evEnd{nullptr}, evRead{nullptr};

    PrefixSumExecution(const Ocl& ocl, const std::vector<uint32_t>& input, std::vector<uint32_t>& output, const OptimalConstants& constants) {
        const size_t logical = input.size();
        const size_t padded = round_up_mul(logical, constants.TILE_SIZE);
        const size_t bytesN = logical * sizeof(uint32_t);
        const size_t bytesP = padded * sizeof(uint32_t);

        cl_int err = CL_SUCCESS;

        bufSrc = MemRAII(clCreateBuffer(ocl.context, CL_MEM_READ_WRITE, bytesP, nullptr, &err));
        check_error(err, "create bufSrc");
        check_error(clEnqueueWriteBuffer(ocl.queue, bufSrc, CL_FALSE, 0, bytesN, input.data(), 0, nullptr, evWrite.get()), "write src");
        pad_block(ocl.queue, bufSrc, logical, padded);
        check_error(clEnqueueMarkerWithWaitList(ocl.queue, 1, evWrite.get(), evStart.get()), "marker start");

        std::vector<Level> levels;
        levels.emplace_back(std::move(bufSrc), logical, constants.TILE_SIZE);

        global_pref_sum(ocl.queue, ocl.context, const_cast<Kernels&>(ocl.kernels), levels[0].data, logical, constants, levels);
        check_error(clEnqueueMarkerWithWaitList(ocl.queue, 0, nullptr, evEnd.get()), "marker end");

        output.resize(logical);
        check_error(clEnqueueReadBuffer(ocl.queue, (cl_mem)levels[0].data, CL_FALSE, 0, bytesN, output.data(), 1, evEnd.get(), evRead.get()), "read dst");
       // check_error(clWaitForEvents(1, evRead.get()), "wait read");
    }

    std::pair<cl_ulong, cl_ulong> getProfilingData(const EventRAII& event) {
        cl_ulong start = 0, end = 0;
        check_error(clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(start), &start, nullptr), "error in prof start");
        check_error(clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(end), &end, nullptr), "error in prof end");
        return {start, end};
    }
};

Timings run_prefix_sum(const Ocl& ocl, const std::vector<uint32_t>& input, std::vector<uint32_t>& output, const OptimalConstants& constants) {
    PrefixSumExecution exec(ocl, input, output, constants);
    cl_event events[] = {exec.evWrite, exec.evStart, exec.evEnd, exec.evRead};    
    check_error(clWaitForEvents(4, events), "error in profiling");
    auto [start_write, end_write] = exec.getProfilingData(exec.evWrite);
    auto [start_kernel, end_kernel] = exec.getProfilingData(exec.evStart);
    auto [start_end, end_end] = exec.getProfilingData(exec.evEnd);
    auto [start_read, end_read] = exec.getProfilingData(exec.evRead);

    Timings t{};
    t.kernel_ms = ns2ms(end_end - start_kernel);
    t.total_ms = ns2ms(end_read - start_write);
    return t;
}
