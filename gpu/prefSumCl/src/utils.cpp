#include "sum.h"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

static int my_cast(const std::string& str, const std::string& param_name) {
    try {
        size_t pos;
        int result = std::stoi(str, &pos);
        if (pos != str.length()) { throw std::invalid_argument("Invalid characters in " + param_name); }
        return result;
    } catch (const std::exception& e) { throw std::invalid_argument("Error parsing " + param_name + " from \"" + str + "\": " + e.what()); }
}

void usage() {
    std::cerr << "lab1.exe < --input file_name > \\" << "\n";
    std::cerr << "         < --output file_name > \\" << "\n";
    std::cerr << "         [ --device-type { dgpu | igpu | gpu | cpu | all } ]" << "\n";
    std::cerr << "         [ --device-index index ]";
}

CommandLineArgs parse_args(int argc, char** argv) {
    CommandLineArgs args;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];

        if (arg == "--input" && i + 1 < argc) {
            args.input_file = argv[++i];
        } else if (arg == "--output" && i + 1 < argc) {
            args.output_file = argv[++i];
        } else if (arg == "--device-type" && i + 1 < argc) {
            args.device_type = argv[++i];
        } else if (arg == "--device-index" && i + 1 < argc) {
            args.device_index = my_cast(argv[++i], "device-index");
        } else if (arg == "--help") {
            usage();
            exit(0);
        } else if (arg == "--tile-size" && i + 1 < argc) {
            args.tile_size = my_cast(argv[++i], "tile-size");
        } else if (arg == "--vec" && i + 1 < argc) {
            args.vec = my_cast(argv[++i], "vec");
        } else {
            throw std::invalid_argument("Unknown argument: " + arg);
        }
    }

    if (args.input_file.empty() || args.output_file.empty()) { throw std::invalid_argument("Missing required arguments"); }
    if (args.device_index < 0) { throw std::invalid_argument("Device index must be non-negative"); }

    return args;
}

void pad_block(cl_command_queue q, MemRAII& buf, size_t logical_len, size_t padded_len) {
    if (padded_len > logical_len) {
        const size_t off = logical_len * sizeof(cl_uint);
        const size_t tail = (padded_len - logical_len) * sizeof(cl_uint);
        fill_zero(q, buf, off, tail);
    }
}

void fill_zero(cl_command_queue q, MemRAII& buf, size_t offset_bytes, size_t size_bytes) {
    if (size_bytes > 0) {
        cl_uint zero = 0;
        check_error(clEnqueueFillBuffer(q, buf, &zero, sizeof(zero), offset_bytes, size_bytes, 0, nullptr, nullptr), "fill_zero");
    }
}

int build_upper_levels(cl_command_queue q, cl_context ctx, Kernels& k, std::vector<Level>& levels, const OptimalConstants& constants) {
    int top = 0;
    while (levels[top].logical_len > constants.TILE_SIZE) {
        const size_t next_len = levels[top].blocks;
        const size_t padded_len = round_up_mul(next_len, constants.TILE_SIZE);
        cl_int err = CL_SUCCESS;
        MemRAII next_buf(clCreateBuffer(ctx, CL_MEM_READ_WRITE, sizeof(cl_uint) * padded_len, nullptr, &err));
        check_error(err, "clCreateBuffer upper level");
        pad_block(q, levels[top].data, levels[top].logical_len, levels[top].padded_len);
        enqueue_block_scan(q, k, levels[top], next_buf, constants);
        levels.emplace_back(std::move(next_buf), next_len, constants.TILE_SIZE);
        ++top;
    }
    return top;
}

void global_pref_sum(cl_command_queue q, cl_context ctx, Kernels& k, MemRAII& src, size_t logical_len, const OptimalConstants& constants, std::vector<Level>& levels) {
    Level& base = levels[0];
    if (base.logical_len <= constants.TILE_SIZE) {
        pad_block(q, base.data, base.logical_len, base.padded_len);
        enqueue_brent_kung(q, k, base.data, constants);
        return;
    }

    int top = build_upper_levels(q, ctx, k, levels, constants);
    Level& top_level = levels[top];
    enqueue_brent_kung(q, k, top_level.data, constants);

    for (int i = top - 1; i >= 0; --i) { enqueue_add_offsets(q, k, levels[i], levels[i + 1].data, constants); }
    return;
}

OptimalConstants calculateOptimalConstants(uint32_t N, const CommandLineArgs& args) {
    OptimalConstants c{};
    c.TILE_SIZE = args.tile_size;
    c.VEC = args.vec;
    return c;
}

std::string generateBuildOpts(const OptimalConstants& c) {
    std::string opts;
    opts += " -DTILE=" + std::to_string(c.TILE_SIZE);
    opts += " -DVEC=" + std::to_string(c.VEC);
    return opts;
}

void print_timings(const Timings& t, const OptimalConstants& constants, const std::string& device_name, const std::string& platform_name) {
    std::cout << "Device: " << device_name << "\tPlatform: " << platform_name << "\n";
    std::cout << "Time:" << t.kernel_ms << "\t" << t.total_ms << "\n";
    std::cout << "TILE_SIZE=" << constants.TILE_SIZE << " VEC=" << constants.VEC << "\n";
    for (const std::string& s : g_kernel_log) { std::cout << s << "\n"; }
}
