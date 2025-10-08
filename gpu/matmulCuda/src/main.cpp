#include <algorithm>
#include <chrono>
#include <cstdint>
#include <iostream>
#include <string>
#include <vector>
#include "mult.h"

int main(int argc, char** argv) {
    try {
        CommandLineArgs args = parse_args(argc, argv);

        uint32_t N = 0, K = 0, M = 0;
        std::vector<float> A, B;
        read_input(args.input_file, N, K, M, A, B);
        // read_text_input(args.input_file, N, K, M, A, B);

        if (A.size() != static_cast<size_t>(M) * K) { throw std::runtime_error("Matrix A size mismatch"); }
        if (B.size() != static_cast<size_t>(K) * N) { throw std::runtime_error("Matrix B size mismatch"); }

        std::vector<float> C;
        try {
            C.resize((size_t)M * N);
        } catch (const std::bad_alloc&) { throw std::runtime_error("Not enough memory to allocate result matrix"); }

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
            } else {
                omp_set_num_threads(threads);
                run_cpu_openmp_realization(A, B, C, M, N, K, args.output_file);
            }
        } else if (args.realization >= 1 && args.realization <= 2) {
            DeviceInfo dev = selectDevice(args.device_type, args.device_index);
            size_t group_size = getMaxGroup(dev.device_index);
            size_t mem_size = getLocalMemSize(dev.device_index);
            OptimalConstants constants = calculateOptimalConstants(M, N, K, args.realization, group_size, mem_size);
            bool enable_padding = true;
            PaddedMatrices padded = padMatrices(A, B, M, N, K, enable_padding, constants.TILE_SIZE);
            Timings t = run_matmul(dev, args, padded.A_padded, padded.B_padded, padded.C_padded, constants);
            extractResult(padded, C, M, N);
            print_timings(t, constants, dev);
            write_output(args.output_file, N, M, C);
            // write_text_output(args.output_file, N, M, C);
        } else {
            throw std::runtime_error("Invalid realization. Use 0, 1, or 2.");
        }
    } catch (const std::exception& e) {
        std::cerr << "error: " << e.what() << "\n";
        return 1;
    }
    return 0;
}