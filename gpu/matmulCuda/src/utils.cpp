#include <algorithm>
#include <chrono>
#include <iostream>
#include <thread>
#include "mult.h"

OptimalConstants calculateOptimalConstants(uint32_t M, uint32_t N, uint32_t K, int realization, size_t max_size, size_t mem_size) {
    OptimalConstants c{};
    c.TILE_SIZE = (realization == 1 ? 16 : 64);

    if (realization == 2) {
        c.vec = 4;
        c.rpi = 4;
    } else {
        c.vec = 1;
        c.rpi = 1;
    }

    while (true) {
        c.local_size_x = c.TILE_SIZE / c.vec;
        c.local_size_y = c.TILE_SIZE / c.rpi;
        bool flag = true;

        size_t group_size = c.local_size_x * c.local_size_y;
        size_t mem_need = 2ull * c.TILE_SIZE * (c.TILE_SIZE + 1) * sizeof(float);
        if (group_size > max_size) flag = false;
        if (mem_need > mem_size) flag = false;

        if (flag) break;

        c.TILE_SIZE /= 2;
    }

    c.M_round = round_up(M, c.TILE_SIZE);
    c.N_round = round_up(N, c.TILE_SIZE);
    c.K_round = round_up(K, c.TILE_SIZE);

    c.groups_x = my_ceil(c.N_round, c.TILE_SIZE);
    c.groups_y = my_ceil(c.M_round, c.TILE_SIZE);
    c.global_size_x = c.groups_x * c.local_size_x;
    c.global_size_y = c.groups_y * c.local_size_y;

    c.block = dim3(c.local_size_x, c.local_size_y);
    c.grid = dim3(c.global_size_x / c.local_size_x, c.global_size_y / c.local_size_y);
    return c;
}

static int my_cast(const std::string& str, const std::string& param_name) {
    try {
        size_t pos;
        int result = std::stoi(str, &pos);
        if (pos != str.length()) { throw std::invalid_argument("Invalid characters in " + param_name); }
        return result;
    } catch (const std::exception& e) { throw std::invalid_argument("Error parsing " + param_name + " from \"" + str + "\": " + e.what()); }
}

CommandLineArgs parse_args(int argc, char** argv) {
    CommandLineArgs args;
    args.omp_threads = omp_get_max_threads();
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
        } else if (arg == "--realization" && i + 1 < argc) {
            args.realization = my_cast(argv[++i], "realization");
        } else if (arg == "--no-omp") {
            args.no_omp = true;
        } else if (arg == "--omp-threads" && i + 1 < argc) {
            std::string val = argv[++i];
            if (val == "default") {
                args.omp_threads = omp_get_max_threads();
                // std::cout << args.omp_threads;
            } else {
                int n = my_cast(val, "omp-threads");
                if (n <= 0) throw std::invalid_argument("threads must be positive");
                args.omp_threads = n;
            }
        } else if (arg == "--help") {
            usage();
            exit(0);
        } else {
            throw std::invalid_argument("Unknown argument: " + arg);
        }
    }

    if (args.input_file.empty() || args.output_file.empty()) { throw std::invalid_argument("Missing required arguments"); }
    if (args.device_index < 0) { throw std::invalid_argument("Device index must be non-negative"); }
    if (args.realization < 0 || args.realization > 2) { throw std::invalid_argument("Realization must be between 0 and 2"); }

    return args;
}

PaddedMatrices padMatrices(const std::vector<float>& A, const std::vector<float>& B, int M, int N, int K, bool enable_padding, int TILE_SIZE) {
    PaddedMatrices result;

    if (!enable_padding) {
        result.A_padded = A;
        result.B_padded = B;
        result.C_padded.resize((size_t)M * N);
        result.M_padded = M;
        result.N_padded = N;
        result.K_padded = K;
        return result;
    }

    result.M_padded = round_up(M, TILE_SIZE);
    result.N_padded = round_up(N, TILE_SIZE);
    result.K_padded = round_up(K, TILE_SIZE);

    result.A_padded.resize((size_t)result.M_padded * result.K_padded, 0.0f);
    result.B_padded.resize((size_t)result.K_padded * result.N_padded, 0.0f);
    result.C_padded.resize((size_t)result.M_padded * result.N_padded, 0.0f);

    for (int i = 0; i < M; ++i) {
        for (int k = 0; k < K; ++k) { result.A_padded[(size_t)i * result.K_padded + k] = A[(size_t)i * K + k]; }
    }

    for (int k = 0; k < K; ++k) {
        for (int j = 0; j < N; ++j) { result.B_padded[(size_t)k * result.N_padded + j] = B[(size_t)k * N + j]; }
    }

    return result;
}

void extractResult(const PaddedMatrices& padded, std::vector<float>& C, int M, int N) {
    C.resize((size_t)M * N);
    for (int i = 0; i < M; ++i) {
        for (int j = 0; j < N; ++j) { C[(size_t)i * N + j] = padded.C_padded[(size_t)i * padded.N_padded + j]; }
    }
}

void print_timings(const Timings& t, OptimalConstants& constants, const DeviceInfo& dev) {
    std::cout << "Device: " << dev.device_name << "\tVersion: " << dev.driverVersion << "\n";
    std::cout << "Time: " << t.kernel_ms << "\t" << t.total_ms << "\n";
    std::cout << "BLOCK_WORK_SIZE [" << constants.block.x << ", " << constants.block.y << "]\n";
    std::cout << "ITEM_WORK_SIZE [" << constants.vec << ", " << constants.rpi << "]\n";
}

void run_cpu_openmp_realization(const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, uint32_t M, uint32_t N, uint32_t K, const std::string& outPath) {
    auto start = std::chrono::high_resolution_clock::now();

    MulMatrixOpenMpSIMD(A.data(), B.data(), C.data(), M, N, K, 32);

    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
    double time_ms = duration.count() / 1e6;

    std::cout << "Time: " << time_ms << "\n";
    write_output(outPath, N, M, C);
    // write_text_output(outPath, N, M, C);
}

void usage() {
    std::cerr << "lab0.exe < --input file_name > \\" << "\n";
    std::cerr << "         < --output file_name > \\" << "\n";
    std::cerr << "         [ --device-type { dgpu | igpu | gpu | cpu | all } ]" << "\n";
    std::cerr << "         [ --device-index index ]" << "\n";
    std::cerr << "         --realization <id>" << "\n";
    std::cerr << "         [ --no-omp | --omp-threads { default | N } ]" << "\n";
    std::cerr << "\n";
    std::cerr << "--realization: 0=CPU OpenMP, 1=Cuda tiled, 2=Cuda tiled+vec" << "\n";
}
