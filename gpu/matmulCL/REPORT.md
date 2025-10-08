| Лабораторная работа №1.1   |  <Группа>                       | Программирование на видеокартах |
| :------------------------- | ------------------------------- | ------------------------------- |
| MatMul.CUDA                | Коршунов Илья Константинович    | 2025                            |

## Введение

В качестве базового варианта (baseline) рассмотрен наивный алгоритм умножения матриц `MulMatrixBaseline`.

```cpp
template <typename T>
void MulMatrixBaseline(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K) {
    for (std::size_t i = 0; i < N; ++i) {
        for (std::size_t j = 0; j < M; ++j) {
            T sum = T(0);
            for (std::size_t k = 0; k < K; ++k) sum += A[i * K + k] * B[k * M + j];
            C[i * M + j] = sum;
        }
    }
}
```

Оптимизации:

1) Перестановка циклов `k, j` для уменьшения числа кэш-промахов (последовательно двигаемся по строкам `B`).

```cpp
template <typename T>
void MulMatrix(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K) {
    for (std::size_t i = 0; i < N; ++i) {
        const std::size_t c_offset = i * M;
        for (std::size_t k = 0; k < K; ++k) {
            const std::size_t b_offset = k * M;
            const T a_val = A[i * K + k];
            for (std::size_t j = 0; j < M; ++j) C[c_offset + j] += a_val * B[b_offset + j];
        }
    }
}
```

2) Блочное умножение для повышения локальности данных (меньше кэш промахов).

```cpp
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
```

3) Векторизация — уменьшение числа итераций за счёт работы с `float2/float4`.

```cpp
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

template <typename T>
void MulMatrixBlockSIMD(const T* A, const T* B, T* C, std::size_t N, std::size_t M, std::size_t K, std::size_t blockSize) {
    MulMatrixBlockGeneric(A, B, C, N, M, K, blockSize, MultiplyBlocksSIMD<T>);
}
```

4) Распараллеливание блоков.

```cpp
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
```

## замеры времени 
Замеры на графиках можно посмотреть по [chart_cpu.png](charts/chart_cpu.png) (static).
Замеры на графиках можно посмотреть по [chart_cpu_dynamic.png](charts/chart_cpu_dynamic.png) (dynamic).

Видно, что dynamic формат дает небольшой выигрыш, но на Вашем сервере предпочтительнее static.

## Профилирование CPU версии
Проведем бенчмарки на матрицах таких размеров (1000 1000 1000)

| Реализация               | D refs            | D1 misses        | LLd misses       | D1 miss rate | LLd miss rate |
|---------------------------|------------------:|-----------------:|-----------------:|-------------:|--------------:|
| Baseline                 | 3,031,287,304     | 1,065,077,545    | 62,951,610       | 35.1%        | 2.1%          |
| MulMatrix                 | 1,781,287,232     | 63,204,540       | 62,950,610       | 3.5%         | 3.5%          |
| MulMatrixBlock (16)       | 2,370,872,368     | 9,435,461        | 4,388,170        | 0.4%         | 0.2%          |
| MulMatrixBlock (32)       | 2,196,666,260     | 7,794,669        | 3,127,869        | 0.4%         | 0.1%          |
| MulMatrixBlock (64)       | 2,111,853,478     | 56,231,281       | 2,405,077        | 2.7%         | 0.1%          |
| MulMatrixOpenMp (16)      | 2,358,715,202     | 13,289,281       | 4,391,312        | 0.6%         | 0.2%          |
## Выводы

D1 в **MulMatrix** сильно выигрывает из-за того, что мы двигаемся линейно по **B**,  
но LLd немного проигрывает, скорее всего, потому что прибавляем **C** в цикле.  

Заметим, что **MulMatrixBlock** в свою очередь намного лучше выигрывает misses, что объясняется выше.  
В целом, ожидаемо, что **MulMatrixOpenMp** немного просядет засчёт того, что каждый поток пишет в свой кэш матрицы + другие приколы многопоточности и планировщика.  

При `size = 64` матрицы выходят за пределы L1-кэша, растут промахи и время.

### Замеры результатов 

Тесты проводились для размеров матриц `100`, `1000`, `10000`.

#### GFLOPS

| Реализация             | 100   | 1000   | 10000  |
|-------------------------|-------|--------|--------|
| MulMatrixBaseline       |  8.12 |  9.68  |  9.35  |
| MulMatrix               | 13.76 | 16.56  | 16.44  |
| MulMatrixBlock (16)     |  8.01 | 18.10  | 17.36  |
| MulMatrixBlock (32)     | 10.59 | 19.42  | 18.51  |
| MulMatrixOpenMp (32)    |  9.28 | 19.47  | 15.92  |
| MulMatrixOpenMp (16)    | 10.09 | 17.61  | 13.39  |

#### Время (ms)

| Реализация             | 100     | 1000     | 10000   |
|-------------------------|---------|----------|---------|
| MulMatrixBaseline       | 0.246   | 206.696  | 213953  |
| MulMatrix               | 0.145   | 120.788  | 121685  |
| MulMatrixBlock (16)     | 0.250   | 110.522  | 115224  |
| MulMatrixBlock (32)     | 0.189   | 102.971  | 108026  |
| MulMatrixOpenMp (32)    | 0.216   | 102.744  | 125600  |
| MulMatrixOpenMp (16)    | 0.198   | 113.549  | 149314  |



### Реализация 2

| tile_size | Время (ms)         | BLOCK_WORK_SIZE | ITEM_WORK_SIZE |
|--------|--------------------|-----------------|----------------|
| 16     | 174.22 / 176.464   | [625, 625]      | [1, 1]         |
| 32     | 124.295 / 126.375  | [313, 313]      | [16, 2]        |

посмотрим 1 реализацию 

| tile_size | Время (ms)       | BLOCK_WORK_SIZE | ITEM_WORK_SIZE |
|--------|------------------|-----------------|----------------|
| 16     | 169.738 / 171.887| [625, 625]      | [1, 1]         |

при tile_size = 32 
error in enqueue kernel (code: -54), то есть не могу выделить стольку групп



реализация 1 на сервере 

### size = 16

| Устройство                                   | Время (ms)       | BLOCK_WORK_SIZE | ITEM_WORK_SIZE |
|----------------------------------------------|------------------|-----------------|----------------|
| AMD gfx1010:xnack- (AMD APP)                 | 8.037 / 11.0294  | [63, 63]        | [1, 1]         |
| Intel UHD Graphics 630 (OpenCL HD Graphics)  | 181.8 / 182.623  | [63, 63]        | [1, 1]         |
| Intel Core i9-9900K CPU @ 3.60GHz (OpenCL)   | 33.3803 / 34.9634| [63, 63]        | [1, 1]         |

### при size =32 code -54

### Реализация 2 (AMD gfx1010:xnack- (dev 0))

#### tile_size = 32

| Время (ms)      | BLOCK_WORK_SIZE | ITEM_WORK_SIZE |
|-----------------|-----------------|----------------|
| 0.94377 / 5.06884 | [32, 32]        | [16, 2]        |

#### tile_size = 64

| Время (ms)      | BLOCK_WORK_SIZE | ITEM_WORK_SIZE |
|-----------------|-----------------|----------------|
| 1.43741 / 5.5265 | [16, 16]        | [16, 2]        |



### Реализация 0 на Вашем сервере(устройство 0). Размер матриц: 1000

| Реализация                 | GFLOPS  | Время (ms) |
|-----------------------------|---------|------------|
| MulMatrixBaseline           | 2.43306 | 822.011    |
| MulMatrix                   | 6.20019 | 322.571    |
| MulMatrixBlock (16)         | 6.28601 | 318.167    |
| MulMatrixBlock (32)         | 6.73180 | 297.097    |
| MulMatrixBlock (64)         | 6.63602 | 301.385    |
| MulMatrixBlockSIMD (32)     | 6.69345 | 298.000    |
| MulMatrixBlockSIMD (128)    | 6.53779 | 305.914    |
| MulMatrixOpenMp (32)        | 6.54968 | 305.358    |
| MulMatrixOpenMpSIMD (32)    | 6.49371 | 307.990    |
| MulMatrixOpenMpSIMD (64)    | 6.79467 | 294.349    |


## openCL

### Результаты замеров и выбор параметров

- **Графики**: [chart_kernels_medium](charts/chart_kernels_medium.png), [chart_kernels_large](charts/chart_kernels_large.png)
- На их основе для реализации 2 выбрано: `vec = 4`, `rpi = 4`, `tile_size = 64`.
- Для реализации 1: `tile_size = 16`.


- Один блок обрабатывает подматрицу `TILE_SIZE × TILE_SIZE`.
- Данные тайлов загружаются в `__local`, затем выполняются вычисления.
- Возможна векторизация по X/Y для роста доли полезных операций.

### Выравнивание и ресурсы

- На хосте выполнено выравнивание размеров => не требуются проверки выхода за границы (нет дивергенции), деления дают целые значения.
- В реализации 1 при `TILE_SIZE = 64` не хватает ресурсов (лимиты shared memory). В реализации 2 это исправляется векторизацией.
- Замеры выполнялись на `NVIDIA GeForce RTX 3060`.

### Невыровненные адреса

- Паддинг до кратности `TILE_SIZE` (транзитивно кратно `VEC`, `RPI`) нужен для отсутствия дивергенции.
- Для `float4` требуются выровненные адреса (16 байт). Иначе возможен ub, либо 716 (misaligned address). Решение:
  - паддинг `__local` до `TILE_SIZE + 4`;
  - в OpenCL `vload4`/`vstore4` допускают невыровненность (ценой производительности), поэтому ошибки не было.


# Первая реализация 

```C
__kernel void matmul_tiled(__global const float* A, __global const float* B, __global float* C, const int M, const int N, const int K) {
    const int local_row = get_local_id(1);
    const int local_col = get_local_id(0);

    const int tile_row_base = get_group_id(1) * TILE_SIZE;
    const int tile_col_base = get_group_id(0) * TILE_SIZE;

    const int c_row = tile_row_base + local_row;
    const int c_col = tile_col_base + local_col;

    __local float tile_a[TILE_SIZE][TILE_SIZE];
    __local float tile_b[TILE_SIZE][TILE_SIZE];

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
```

# Вторая реализация 


```C
__kernel void matmul_tiled_vecN(__global const float* restrict A, __global const float* restrict B, __global float* restrict C, const int M, const int N, const int K) {
    const int local_row = get_local_id(1);
    const int local_group = get_local_id(0);

    const int tile_row_base = get_group_id(1) * TILE_SIZE + local_row * RPI;
    const int tile_col_base = get_group_id(0) * TILE_SIZE + local_group * VEC;

    __local float tile_a[TILE_SIZE][TILE_SIZE + 1];
    __local float tile_b[TILE_SIZE][TILE_SIZE + 1];

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

    // снижаю нагрузку на регистры, благодаря 2 разным for 

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
    // снижаю нагрузку на регистры, благодаря 2 разным for 
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
```