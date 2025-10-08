#include <cuda_runtime.h>
#include <stdexcept>
#include <string>
#include <vector>
#include "mult.h"

size_t getMaxGroup(int device) {
    cudaDeviceProp prop{};
    cuda_check(cudaGetDeviceProperties(&prop, device), "failed to get device properties");
    return static_cast<size_t>(prop.maxThreadsPerBlock);
}

size_t getLocalMemSize(int device) {
    cudaDeviceProp prop{};
    cuda_check(cudaGetDeviceProperties(&prop, device), "failed to get device properties");
    return static_cast<size_t>(prop.sharedMemPerBlock);
}

struct KernelExecution {
    CudaEventRaii evWriteAStart, evWriteAEnd;
    CudaEventRaii evWriteBStart, evWriteBEnd;
    CudaEventRaii evKernelStart, evKernelEnd;
    CudaEventRaii evReadStart, evReadEnd;

    KernelExecution(const MatmulBuffers& buffers, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, OptimalConstants& constants, int realization) {
        cudaEventRecord(evWriteAStart);
        cuda_check(cudaMemcpy(buffers.dA, A.data(), buffers.bytesA, cudaMemcpyHostToDevice), "error cudaMemcpy A");
        cudaEventRecord(evWriteAEnd);

        cudaEventRecord(evWriteBStart);
        cuda_check(cudaMemcpy(buffers.dB, B.data(), buffers.bytesB, cudaMemcpyHostToDevice), "error cudaMemcpy B");
        cudaEventRecord(evWriteBEnd);

        cudaEventRecord(evKernelStart);
        if (realization == 1) {
            launch_matmul_tile(buffers.dA, buffers.dB, buffers.dC, constants.M_round, constants.N_round, constants.K_round, constants.grid, constants.block, constants.TILE_SIZE);
        } else {
            launch_matmul_tiled_vecN(buffers.dA, buffers.dB, buffers.dC, constants.M_round, constants.N_round, constants.K_round, constants.grid, constants.block, constants);
        }
        cuda_check(cudaGetLastError(), "error kernel");
        cuda_check(cudaDeviceSynchronize(), "error kernel sync");
        cudaEventRecord(evKernelEnd);

        C.resize((size_t)constants.M_round * constants.N_round);
        cudaEventRecord(evReadStart);
        cuda_check(cudaMemcpy(C.data(), buffers.dC, buffers.bytesC, cudaMemcpyDeviceToHost), "error cudaMemcpy C");
        cudaEventRecord(evReadEnd);
        cuda_check(cudaDeviceSynchronize(), "error events sync");
    }

    Timings getTimings(int M, int N, int K) {
        float ms_kernel = 0.0f;
        cudaEventElapsedTime(&ms_kernel, evKernelStart, evKernelEnd);

        float ms_writeA = 0.0f, ms_writeB = 0.0f, ms_read = 0.0f;
        cudaEventElapsedTime(&ms_writeA, evWriteAStart, evWriteAEnd);
        cudaEventElapsedTime(&ms_writeB, evWriteBStart, evWriteBEnd);
        cudaEventElapsedTime(&ms_read, evReadStart, evReadEnd);

        Timings t{};
        t.kernel_ms = ms_kernel;
        t.total_ms = ms_writeA + ms_writeB + ms_kernel + ms_read;
        double ksec = ms_kernel * 1e-3;
        t.gflops = (2.0 * (double)M * (double)N * (double)K / ksec / 1e9);
        return t;
    }
};

Timings run_matmul(const DeviceInfo& dev, const CommandLineArgs& args, const std::vector<float>& A, const std::vector<float>& B, std::vector<float>& C, OptimalConstants& constants) {
    cuda_check(cudaSetDevice(dev.device_index), "error cudaSetDevice");
    MatmulBuffers buffers(constants.M_round, constants.N_round, constants.K_round);
    KernelExecution exec(buffers, A, B, C, constants, args.realization);
    return exec.getTimings(constants.M_round, constants.N_round, constants.K_round);
}