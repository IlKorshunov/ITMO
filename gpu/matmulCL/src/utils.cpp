#include <algorithm>
#include <chrono>
#include "mult.h"
#include <omp.h>

OptimalConstants calculateOptimalConstants(uint32_t M, uint32_t N, uint32_t K, int realization, size_t max_size, size_t mem_size, int vec, int rpi, int tile_size) {
    OptimalConstants c{};
    long long ops = 2LL * M * N * K;
    
    if (tile_size != -1) c.TILE_SIZE = tile_size;
    else c.TILE_SIZE = (realization == 1 ? 16 : 64);
    
    if (rpi != -1) c.rpi = rpi;
    else c.rpi = (realization == 2 ? 4 : 1);
    
    if (vec != -1) c.vec = vec;
    else c.vec = (realization == 2 ? 4 : 1);
    

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

    c.groups_x = c.N_round / c.TILE_SIZE; 
    c.groups_y = c.M_round / c.TILE_SIZE;
    c.global_size_x = c.groups_x * c.local_size_x;
    c.global_size_y = c.groups_y * c.local_size_y;

    return c;
}

std::string generateBuildOpts(const OptimalConstants& c) {
    std::string opts;
    opts += " -DTILE_SIZE=" + std::to_string(c.TILE_SIZE);
    opts += " -DVEC=" + std::to_string(c.vec);
    opts += " -DRPI=" + std::to_string(c.rpi);

    return opts;
}

const char* getKernelName(int realization) {
    switch (realization) {
    case 1: return "matmul_tiled";
    case 2: return "matmul_tiled_vecN";
    default: throw std::runtime_error("Invalid realization for kernel name");
    }
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
        } else if (arg == "--vec" && i + 1 < argc) {
            args.vec = my_cast(argv[++i], "vec");
        } else if (arg == "--rpi" && i + 1 < argc) {
            args.rpi = my_cast(argv[++i], "rpi");
        } else if (arg == "--tile-size" && i + 1 < argc) {
            args.tile_size = my_cast(argv[++i], "tile-size");
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

void padMatrices(const std::vector<float>& A, const std::vector<float>& B, uint32_t M, uint32_t N, uint32_t K, std::vector<float>& A_padded, std::vector<float>& B_padded, std::vector<float>& C_padded, uint32_t& M_padded, uint32_t& N_padded, uint32_t& K_padded) {
    try {
        A_padded.resize((size_t)M_padded * K_padded, 0.0f);
        B_padded.resize((size_t)K_padded * N_padded, 0.0f);
        C_padded.resize((size_t)M_padded * N_padded, 0.0f);
    } catch (const std::bad_alloc& e) { throw std::runtime_error("Not enough memory to allocate padded matrices"); }

    for (uint32_t i = 0; i < M; ++i) {
        for (uint32_t k = 0; k < K; ++k) { A_padded[i * K_padded + k] = A[i * K + k]; }
    }

    for (uint32_t k = 0; k < K; ++k) {
        for (uint32_t j = 0; j < N; ++j) { B_padded[k * N_padded + j] = B[k * N + j]; }
    }
}

void print_timings(const Timings& t, const OptimalConstants& constants, const std::string& device_name, const std::string& platform_name) {
    std::cout << "Device: " << device_name << "\tPlatform: " << platform_name << "\n";
    std::cout << "Time:" << t.kernel_ms << "\t" << t.total_ms << "\n";
    // std::cout << "GFLOPS: " << t.gflops << "\n";
    std::cout << "BLOCK_WORK_SIZE [" << constants.local_size_x << ", " << constants.local_size_y << "]\n";
    std::cout << "ITEM_WORK_SIZE [" << constants.vec << ", " << constants.rpi << "]\n";
}

void run_cpu_openmp_realization(const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, uint32_t M, uint32_t N, uint32_t K, const std::string& outPath) {
    auto start = std::chrono::high_resolution_clock::now();

    MulMatrixOpenMpSIMD(A.data(), B.data(), C.data(), M, N, K, 64);

    auto end = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
    double time_ms = duration.count() / 1e6;

    std::cout << "Time: " << time_ms << "\n";
    write_output(outPath, N, M, C);
    //write_text_output(outPath, N, M, C);
}
void usage() {
    std::cerr << "lab0.exe < --input file_name > \\" << "\n";
    std::cerr << "         < --output file_name > \\" << "\n";
    std::cerr << "         [ --device-type { dgpu | igpu | gpu | cpu | all } ]" << "\n";
    std::cerr << "         [ --device-index index ]" << "\n";
    std::cerr << "         --realization <id>" << "\n";
    std::cerr << "\n";
    std::cerr << "--realization: 0=CPU OpenMP, 1=OpenCL tiled, 2=OpenCL tiled+vec" << "\n";
}
