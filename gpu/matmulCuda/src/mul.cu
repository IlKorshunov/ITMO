#include "mult.h"

template <int VEC>
struct VecType;

template <>
struct VecType<2> {
    using type = float2;
};

template <>
struct VecType<4> {
    using type = float4;
};

template <int VEC>
using vec_t = typename VecType<VEC>::type;

template <int VEC>
__device__ vec_t<VEC> load(const float* ptr) {
    return *reinterpret_cast<const vec_t<VEC>*>(ptr);
}

template <int VEC>
__device__ void store(vec_t<VEC> v, float* ptr) {
    *reinterpret_cast<vec_t<VEC>*>(ptr) = v;
}

template <int VEC>
__device__ vec_t<VEC> make_vec(float v);

template <>
__device__ float2 make_vec<2>(float v) {
    return make_float2(v, v);
}

template <>
__device__ float4 make_vec<4>(float v) {
    return make_float4(v, v, v, v);
}

template <int VEC>
__device__ vec_t<VEC> vec_mul(const vec_t<VEC>& v, float a);

template <>
__device__ float2 vec_mul<2>(const float2& v, float a) {
    return make_float2(v.x * a, v.y * a);
}

template <>
__device__ float4 vec_mul<4>(const float4& v, float a) {
    return make_float4(v.x * a, v.y * a, v.z * a, v.w * a);
}

template <int VEC>
__device__ vec_t<VEC> vec_add(const vec_t<VEC>& x, const vec_t<VEC>& y);

template <>
__device__ float2 vec_add<2>(const float2& x, const float2& y) {
    return make_float2(x.x + y.x, x.y + y.y);
}

template <>
__device__ float4 vec_add<4>(const float4& x, const float4& y) {
    return make_float4(x.x + y.x, x.y + y.y, x.z + y.z, x.w + y.w);
}

template <int TILE_SIZE, int VEC, int RPI>
__global__ void matmul_tiled_vecN(const float* __restrict__ A, const float* __restrict__ B, float* __restrict__ C, const int M, const int N, const int K) {
    const int local_row = threadIdx.y;
    const int local_col = threadIdx.x;

    const int tile_row_base = blockIdx.y * TILE_SIZE + local_row * RPI;
    const int tile_col_base = blockIdx.x * TILE_SIZE + local_col * VEC;

    __shared__ float tile_a[TILE_SIZE][TILE_SIZE + 4];
    __shared__ float tile_b[TILE_SIZE][TILE_SIZE + 4];

    vec_t<VEC> acc[RPI];
    #pragma unroll
    for (int r = 0; r < RPI; ++r) { acc[r] = make_vec<VEC>(0.0f); }

    const int tile_count = K / TILE_SIZE;

    for (int t = 0; t < tile_count; ++t) {
        const int tile_k_base = t * TILE_SIZE;

        #pragma unroll
        for (int r = 0; r < RPI; ++r) {
            const int a_row = tile_row_base + r;
            const int a_col_base = tile_k_base + local_col * VEC;
            auto a_vec = load<VEC>(&A[(size_t)a_row * K + a_col_base]);
            const int local_row_in_tile = local_row * RPI + r;
            store<VEC>(a_vec, &tile_a[local_row_in_tile][local_col * VEC]);
        }

        #pragma unroll
        for (int r = 0; r < RPI; ++r) {
            const int b_row = tile_k_base + local_row * RPI + r;
            auto b_vec = load<VEC>(&B[(size_t)b_row * N + tile_col_base]);
            const int local_row_in_tile = local_row * RPI + r;
            store<VEC>(b_vec, &tile_b[local_row_in_tile][local_col * VEC]);
        }

        __syncthreads();

        const int col_base = local_col * VEC;

        if constexpr (TILE_SIZE == 64) {
            #pragma unroll
            for (int k = 0; k < 32; ++k) {
                auto b = load<VEC>(&tile_b[k][col_base]);
                #pragma unroll
                for (int r = 0; r < RPI; ++r) {
                    const int local_row_in_tile = local_row * RPI + r;
                    float a = tile_a[local_row_in_tile][k];
                    acc[r] = vec_add<VEC>(acc[r], vec_mul<VEC>(b, a));
                }
            }

            #pragma unroll
            for (int k = 32; k < 64; ++k) {
                auto b = load<VEC>(&tile_b[k][col_base]);
                #pragma unroll
                for (int r = 0; r < RPI; ++r) {
                    const int local_row_in_tile = local_row * RPI + r;
                    float a = tile_a[local_row_in_tile][k];
                    acc[r] = vec_add<VEC>(acc[r], vec_mul<VEC>(b, a));
                }
            }
        } else {
            #pragma unroll
            for (int k = 0; k < TILE_SIZE; ++k) {
                auto b = load<VEC>(&tile_b[k][col_base]);
                #pragma unroll
                for (int r = 0; r < RPI; ++r) {
                    const int local_row_in_tile = local_row * RPI + r;
                    float a = tile_a[local_row_in_tile][k];
                    acc[r] = vec_add<VEC>(acc[r], vec_mul<VEC>(b, a));
                }
            }
        }

        __syncthreads();
    }

    #pragma unroll
    for (int r = 0; r < RPI; ++r) {
        const int c_row = tile_row_base + r;
        store<VEC>(acc[r], &C[(size_t)c_row * N + tile_col_base]);
    }
}

template <int TILE_SIZE>
__global__ void matmul_tile(const float* __restrict__ A, const float* __restrict__ B, float* __restrict__ C, int M, int N, int K) {
    const int local_row = threadIdx.y;
    const int local_col = threadIdx.x;

    const int tile_row = blockIdx.y * TILE_SIZE;
    const int tile_col = blockIdx.x * TILE_SIZE;

    const int c_row = tile_row + local_row;
    const int c_col = tile_col + local_col;

    __shared__ float tile_a[TILE_SIZE][TILE_SIZE + 4];
    __shared__ float tile_b[TILE_SIZE][TILE_SIZE + 4];

    float acc = 0.0f;

    const int tile_count = K / TILE_SIZE;

    for (int cur_tile = 0; cur_tile < tile_count; cur_tile++) {
        const int a_col = cur_tile * TILE_SIZE + local_col;
        const int b_row = cur_tile * TILE_SIZE + local_row;

        tile_a[local_row][local_col] = A[c_row * K + a_col];
        tile_b[local_row][local_col] = B[b_row * N + c_col];

        __syncthreads();

        #pragma unroll
        for (int k = 0; k < TILE_SIZE; k++) { acc += tile_a[local_row][k] * tile_b[k][local_col]; }
        __syncthreads();
    }

    C[c_row * N + c_col] = acc;
}

void launch_matmul_tile(const float* A, const float* B, float* C, int M, int N, int K, dim3 grid, dim3 block, int tile_size) {
    if (tile_size == 16) {
        matmul_tile<16><<<grid, block>>>(A, B, C, M, N, K);
    } else {
        matmul_tile<32><<<grid, block>>>(A, B, C, M, N, K);
    }
}

void launch_matmul_tiled_vecN(const float* A, const float* B, float* C, int M, int N, int K, dim3 grid, dim3 block, const OptimalConstants& c) {
    if (c.TILE_SIZE == 64 && c.vec == 4 && c.rpi == 1) {
        matmul_tiled_vecN<64, 4, 1><<<grid, block>>>(A, B, C, M, N, K);
    } else if (c.TILE_SIZE == 64 && c.vec == 4 && c.rpi == 4) {
        matmul_tiled_vecN<64, 4, 4><<<grid, block>>>(A, B, C, M, N, K);
    } else if (c.TILE_SIZE == 64 && c.vec == 4 && c.rpi == 16) {
        matmul_tiled_vecN<64, 4, 16><<<grid, block>>>(A, B, C, M, N, K);
    } else if (c.TILE_SIZE == 64 && c.vec == 2 && c.rpi == 4) {
        matmul_tiled_vecN<64, 2, 4><<<grid, block>>>(A, B, C, M, N, K);
    } else if (c.TILE_SIZE == 64 && c.vec == 2 && c.rpi == 16) {
        matmul_tiled_vecN<64, 2, 16><<<grid, block>>>(A, B, C, M, N, K);
    } else if (c.TILE_SIZE == 32 && c.vec == 4 && c.rpi == 16) {
        matmul_tiled_vecN<32, 4, 16><<<grid, block>>>(A, B, C, M, N, K);
    } else {
        matmul_tiled_vecN<32, 4, 4><<<grid, block>>>(A, B, C, M, N, K);
    }
}
