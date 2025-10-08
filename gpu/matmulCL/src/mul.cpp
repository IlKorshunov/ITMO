#include <algorithm>
#include <cstddef>
#include <type_traits>
#include <omp.h>

#if defined(__AVX__)
#include <immintrin.h>
#define SIMD_AVAILABLE 1
#else
#define SIMD_AVAILABLE 0
#endif

template <typename T>
void MulMatrixBaseline(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K) {
    for (std::size_t i = 0; i < N; ++i) {
        for (std::size_t j = 0; j < M; ++j) {
            T sum = T(0);
            for (std::size_t k = 0; k < K; ++k) { sum += A[i * K + k] * B[k * M + j]; }
            C[i * M + j] = sum;
        }
    }
}

template <typename T>
void MulMatrix(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K) {
    for (std::size_t i = 0; i < N; ++i) {
        size_t c_offset = i * M;
        for (std::size_t k = 0; k < K; ++k) {
            size_t b_offset = k * M;
            T a_val = A[i * K + k];
            for (std::size_t j = 0; j < M; ++j) { C[c_offset + j] += a_val * B[b_offset + j]; }
        }
    }
}

template <typename T>
void MultiplyBlocks(const T* A, const T* B, T* C, std::size_t iStart, std::size_t iEnd, std::size_t kStart, std::size_t kEnd, std::size_t jStart, std::size_t jEnd, std::size_t M, std::size_t K) {
    for (std::size_t i = iStart; i < iEnd; ++i) {
        size_t c_offset = i * M;
        for (std::size_t k = kStart; k < kEnd; ++k) {
            size_t b_offset = k * M;
            const T a_val = A[i * K + k];
            for (std::size_t j = jStart; j < jEnd; ++j) { C[c_offset + j] += a_val * B[b_offset + j]; }
        }
    }
}

template <typename T>
void MultiplyBlocksSIMD(const T* A, const T* B, T* C, std::size_t iStart, std::size_t iEnd, std::size_t kStart, std::size_t kEnd, std::size_t jStart, std::size_t jEnd, std::size_t M, std::size_t K) {
    for (std::size_t i = iStart; i < iEnd; ++i) {
        size_t c_offset = i * M;

        for (std::size_t k = kStart; k < kEnd; ++k) {
            size_t b_offset = k * M;
            const T a_val = A[i * K + k];

            std::size_t j = jStart;

            #if SIMD_AVAILABLE
            __m256 a_vec = _mm256_set1_ps(a_val);
            for (; j + 8 <= jEnd; j += 8) {
                __m256 b_vec = _mm256_loadu_ps(B + b_offset + j);
                __m256 c_vec = _mm256_loadu_ps(C + c_offset + j);
                c_vec = _mm256_add_ps(_mm256_mul_ps(a_vec, b_vec), c_vec);
                _mm256_storeu_ps(C + c_offset + j, c_vec);
            }
            #else
            for (; j < jEnd; ++j) { C[c_offset + j] += a_val * B[b_offset + j]; }
            #endif
        }
    }
}

template <typename T, typename BlockFunc>
void MulMatrixBlockGeneric(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize, BlockFunc blockFunc) {
    for (std::size_t i0 = 0; i0 < N; i0 += blockSize) {
        const std::size_t iEnd = std::min(i0 + blockSize, N);
        for (std::size_t k0 = 0; k0 < K; k0 += blockSize) {
            const std::size_t kEnd = std::min(k0 + blockSize, K);
            for (std::size_t j0 = 0; j0 < M; j0 += blockSize) {
                const std::size_t jEnd = std::min(j0 + blockSize, M);
                blockFunc(A, B, C, i0, iEnd, k0, kEnd, j0, jEnd, M, K);
            }
        }
    }
}

template <typename T, typename BlockFunc>
void MulMatrixOpenMpGeneric(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize, BlockFunc blockFunc) {
#pragma omp parallel for collapse(2) schedule(static)
    for (std::size_t i0 = 0; i0 < N; i0 += blockSize) {
        for (std::size_t j0 = 0; j0 < M; j0 += blockSize) {
            const std::size_t iEnd = std::min(i0 + blockSize, N);
            const std::size_t jEnd = std::min(j0 + blockSize, M);
            for (std::size_t k0 = 0; k0 < K; k0 += blockSize) {
                const std::size_t kEnd = std::min(k0 + blockSize, K);
                blockFunc(A, B, C, i0, iEnd, k0, kEnd, j0, jEnd, M, K);
            }
        }
    }
}

template <typename T>
void MulMatrixBlock(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize) {
    MulMatrixBlockGeneric(A, B, C, N, M, K, blockSize, MultiplyBlocks<T>);
}

template <typename T>
void MulMatrixBlockSIMD(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize) {
    MulMatrixBlockGeneric(A, B, C, N, M, K, blockSize, MultiplyBlocksSIMD<T>);
}

template <typename T>
void MulMatrixOpenMpSIMD(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize) {
    MulMatrixOpenMpGeneric(A, B, C, N, M, K, blockSize, MultiplyBlocksSIMD<T>);
}

template <typename T>
void MulMatrixOpenMp(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize) {
    MulMatrixOpenMpGeneric(A, B, C, N, M, K, blockSize, MultiplyBlocks<T>);
}

template void MulMatrixBlock<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template void MulMatrixBlockSIMD<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template void MulMatrixOpenMp<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template void MulMatrixOpenMpSIMD<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize);
template void MulMatrixBaseline<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K);
template void MulMatrix<float>(const float* A, const float* B, float* C, std::size_t N, std::size_t M, std::size_t K);