#ifndef TILE_SIZE
#define TILE_SIZE 16
#endif

#ifndef VEC
#define VEC 4
#endif
#ifndef RPI
#define RPI 1
#endif

#if (VEC == 1)
typedef float vec_t;
#define VLOAD(ptr) (*(ptr))
#define VSTORE(v, p) (*(p) = (v))
#elif (VEC == 2)
typedef float2 vec_t;
#define VLOAD(ptr) vload2(0, (ptr))
#define VSTORE(v, p) vstore2((v), 0, (p))
#elif (VEC == 4)
typedef float4 vec_t;
#define VLOAD(ptr) vload4(0, (ptr))
#define VSTORE(v, p) vstore4((v), 0, (p))
#elif (VEC == 8)
typedef float8 vec_t;
#define VLOAD(ptr) vload8(0, (ptr))
#define VSTORE(v, p) vstore8((v), 0, (p))
#elif (VEC == 16)
typedef float16 vec_t;
#define VLOAD(ptr) vload16(0, (ptr))
#define VSTORE(v, p) vstore16((v), 0, (p))
#endif

__kernel void matmul_tiled_vecN(__global const float* restrict A, __global const float* restrict B, __global float* restrict C, const int M, const int N, const int K) {
    const int local_row = get_local_id(1);
    const int local_group = get_local_id(0);

    const int tile_row_base = get_group_id(1) * TILE_SIZE + local_row * RPI;
    const int tile_col_base = get_group_id(0) * TILE_SIZE + local_group * VEC;

    __local float tile_a[TILE_SIZE][TILE_SIZE + 4];
    __local float tile_b[TILE_SIZE][TILE_SIZE + 4];

    vec_t acc[RPI];
    #pragma unroll
    for (int r = 0; r < RPI; ++r) { acc[r] = (vec_t)(0.0f); }

    const int tile_count = K / TILE_SIZE;

    for (int t = 0; t < tile_count; ++t) {
        const int tile_k_base = t * TILE_SIZE;

        #pragma unroll
        for (int r = 0; r < RPI; ++r) {
            const int a_row = tile_row_base + r;
            const int a_col_base = tile_k_base + local_group * VEC;
            vec_t a_vec = VLOAD(&A[(size_t)a_row * (size_t)K + (size_t)a_col_base]);
            const int local_row_in_tile = local_row * RPI + r;
            VSTORE(a_vec, &tile_a[local_row_in_tile][local_group * VEC]);
        }

        #pragma unroll
        for (int r = 0; r < RPI; ++r) {
            const int b_row = tile_k_base + local_row * RPI + r;
            vec_t b_vec = VLOAD(&B[(size_t)b_row * (size_t)N + (size_t)tile_col_base]);
            const int local_row_in_tile = local_row * RPI + r;
            VSTORE(b_vec, &tile_b[local_row_in_tile][local_group * VEC]);
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        const int col_base = local_group * VEC;

    #if TILE_SIZE == 64
        #pragma unroll
        for (int k = 0; k < 32; ++k) {
            vec_t b = VLOAD(&tile_b[k][col_base]);
            #pragma unroll
            for (int r = 0; r < RPI; ++r) {
                const int local_row_in_tile = local_row * RPI + r;
                float a = tile_a[local_row_in_tile][k];
                acc[r] += b * a;
            }
        }
        #pragma unroll
        for (int k = 32; k < 64; ++k) {
            vec_t b = VLOAD(&tile_b[k][col_base]);
            #pragma unroll
            for (int r = 0; r < RPI; ++r) {
                const int local_row_in_tile = local_row * RPI + r;
                float a = tile_a[local_row_in_tile][k];
                acc[r] += b * a;
            }
        }
    #else

        #pragma unroll
        for (int k = 0; k < TILE_SIZE; ++k) {
            vec_t b = VLOAD(&tile_b[k][col_base]);
            #pragma unroll
            for (int r = 0; r < RPI; ++r) {
                const int local_row_in_tile = local_row * RPI + r;
                float a = tile_a[local_row_in_tile][k];
                acc[r] += b * a;
            }
        }
    #endif


        barrier(CLK_LOCAL_MEM_FENCE);
    }

    #pragma unroll
    for (int r = 0; r < RPI; ++r) {
        const int c_row = tile_row_base + r;
        VSTORE(acc[r], &C[(size_t)c_row * (size_t)N + (size_t)tile_col_base]);
    }
}

__kernel void matmul_tiled(__global const float* A, __global const float* B, __global float* C, const int M, const int N, const int K) {
    const int local_row = get_local_id(1);
    const int local_col = get_local_id(0);

    const int tile_row_base = get_group_id(1) * TILE_SIZE;
    const int tile_col_base = get_group_id(0) * TILE_SIZE;

    const int c_row = tile_row_base + local_row;
    const int c_col = tile_col_base + local_col;

    __local float tile_a[TILE_SIZE][TILE_SIZE+1];
    __local float tile_b[TILE_SIZE][TILE_SIZE+1];

    float acc = 0.0f;

    const int tile_count = K / TILE_SIZE;
    
    for (int t = 0; t < tile_count; ++t) {
        const int a_col = t * TILE_SIZE + local_col;
        const int b_row = t * TILE_SIZE + local_row;

        tile_a[local_row][local_col] = A[c_row * K + a_col];
        tile_b[local_row][local_col] = B[b_row * N + c_col];

        barrier(CLK_LOCAL_MEM_FENCE);

        #pragma unroll
        for (int k = 0; k < TILE_SIZE; ++k) { acc += tile_a[local_row][k] * tile_b[k][local_col]; }

        barrier(CLK_LOCAL_MEM_FENCE);
    }

    C[c_row * N + c_col] = acc;
}