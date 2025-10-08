#include <algorithm>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>
#include "mult.h"
#include <omp.h>

int main(int argc, char** argv) {
    try {
        CommandLineArgs args = parse_args(argc, argv);

        uint32_t N = 0, K = 0, M = 0;
        std::vector<float> A, B;
        read_input(args.input_file, N, K, M, A, B);
        //read_text_input(args.input_file, N, K, M, A, B);

        if (A.size() != static_cast<size_t>(M) * K) { throw std::runtime_error("Matrix A size mismatch"); }
        if (B.size() != static_cast<size_t>(K) * N) { throw std::runtime_error("Matrix B size mismatch"); }

        std::vector<float> C;
        try {
            C.resize((size_t)M * N);
        } catch (const std::bad_alloc& e) { throw std::runtime_error("Not enough memory to allocate result matrix"); }

        if (args.realization == 0) {
            int threads = args.no_omp ? 1 : args.omp_threads;
            if (threads == 1) {
                auto start = std::chrono::high_resolution_clock::now();
                MulMatrix<float>(A.data(), B.data(), C.data(), M, N, K);
                auto end = std::chrono::high_resolution_clock::now();
                auto duration = std::chrono::duration_cast<std::chrono::nanoseconds>(end - start);
                double time_ms = duration.count() / 1e6;
                std::cout << "Time: " << time_ms << "\n";
                write_output(args.output_file, N, M, C);
                //write_text_output(args.output_file, N, M, C);
            } else {
                omp_set_num_threads(threads);
                run_cpu_openmp_realization(A, B, C, M, N, K, args.output_file);
            }
        } else if (args.realization >= 1 && args.realization <= 2) {
            DeviceInfo selected_device = selectDevice(args.device_type, args.device_index);
            size_t max_size = getMaxGroup(selected_device.device);
            size_t mem_size = getLocalMemSize(selected_device.device);
            OptimalConstants constants = calculateOptimalConstants(M, N, K, args.realization, max_size, mem_size, args.vec, args.rpi, args.tile_size);
            const char* kernel_name = getKernelName(args.realization);
            std::string build_opts = generateBuildOpts(constants);

            std::vector<float> A_padded, B_padded, C_padded;
            padMatrices(A, B, M, N, K, A_padded, B_padded, C_padded, constants.M_round, constants.N_round, constants.K_round);

            Ocl ocl = Ocl::build(selected_device, "matmul.cl", kernel_name, build_opts.c_str());

            Timings t = run_matmul(ocl, A_padded, B_padded, C_padded, constants);

            for (uint32_t i = 0; i < M; ++i) {
                for (uint32_t j = 0; j < N; ++j) { C[i * N + j] = C_padded[i * constants.N_round + j]; }
            }

            print_timings(t, constants, ocl.device_name, ocl.platform_name);
            write_output(args.output_file, N, M, C);
            //write_text_output(args.output_file, N, M, C);
        } else {
            throw std::runtime_error("Invalid realization. Use 0, 1, 2.");
        }

    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
    return 0;
}