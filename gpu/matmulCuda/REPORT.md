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

2) Блочное умножение для повышения локальности данных.

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

3) Векторизация — уменьшение числа итераций засчёт работы с `float2/float4`.

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

Видно, что dynamic формат дает небольшой выигрыш на моем сервере, но на Вашем предпочтительнее выбрать static.

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

## CUDA 

Есть shared память, находящаяся на чипе => к ней куда быстрее доступ.
на GPU есть warp из 32 потоков - все потоки в варпе выполняют одну и ту же инструкцию одновременно. При наличии ветвления (if/else) возникает дивергенция: потоки, удовлетворяющие разным условиям, вынуждены выполняться последовательно, что приводит к простоям части потоков варпа.

1 блок обрабатывает матрицу TILE_SIZE * TILE_SIZE, загрузим матрицы в shared, а затем проведем расчеты.
Также обработку можно делать векторно по x/y для увеличения процента полезных действий. 

Примечание: на хосте сделано выравнивание, поэтому не надо проверять выходы за пределы с помощью if(нет дивергенции), а также быть спокойным, что все деления будут давать целые результаты. 

В первой реализации при TILE_SIZE = 64 не хватает ресурсов для запуска ядра, так как превышение лимитов в shared memory. Во второй реализации это чинится векторизацией => меньше размер блока.

__restrict__ нужен, чтобы компилятор мог оптимизировать операции с памятью, при этом надо гарантировать, что этот указатель не пересекается с другими 

```c
template<int TILE_SIZE, int VEC, int RPI>
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

        // снижаю давление на регистры, разнеся по разным for.

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
        // разбил на 2 for, чтобы уменьшить нагрузку на регистры.
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
```

## Профилирование GPU с помощью NCU

- Замеры выполнялись на `NVIDIA GeForce RTX 3060`.
- Паддинг матриц до кратности `TILE_SIZE, VEC, RPI` (в действительности до TILE_SIZE, т.к. остальное по транзитивности будет кратно) необходимо для отсутствия дивергнеции из-за проверки на выход за пределы массива.
- Для векторных типов (`float4`) необходимы выровненные адреса (16 байт). В противном случае в CUDA при векторных операциях возникает ub, некоторые устройства кидают ошибку 716 (misaligned address). Это решается:
  - паддингом shared массивов до `TILE_SIZE + 4`.
  - в OpenCL `vload4/vstore4` допускают невыровненные адреса (ценой производительности), поэтому там подобной ошибки не было.

Device: NVIDIA GeForce RTX 3060  Version: 12020
Time: 815.159   817.368
BLOCK_WORK_SIZE [16, 16]
ITEM_WORK_SIZE [4, 4]

Memory Throughput: 76.13%
Compute (SM) Throughput: 50.28%
Achieved Occupancy: 31.80%
Registers Per Thread: 49
L2 Hit Rate: 79.15%
Executed IPC: 2.01 inst/cycle
L1/TEX Hit Rate: 0.93%
Shared Memory Per Block: 34,816 bytes
Theoretical Occupancy: 33.33%
Shared Memory Bank Conflicts: 18.44%
Warp Cycles Per Instruction: 7.04 cycles
DRAM Throughput: 17.86%
SM Busy: 54.00%

Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 1130.11   1132.34
BLOCK_WORK_SIZE [16, 4]
ITEM_WORK_SIZE [4, 16]

Memory Throughput: 37.38%
Compute (SM) Throughput: 40.67%
Achieved Occupancy: 8.41%
Registers Per Thread: 144
L2 Hit Rate: 75.92%
Executed IPC: 1.63 inst/cycle
L1/TEX Hit Rate: 21.59%
Shared Memory Per Block: 34,816 bytes
Theoretical Occupancy: 8.33%
Warp Cycles Per Instruction: 2.26 cycles
DRAM Throughput: 13.97%
SM Busy: 44.61%

Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 910.743   912.986
BLOCK_WORK_SIZE [16, 64]
ITEM_WORK_SIZE [4, 1]

Memory Throughput: 80.49%
Compute (SM) Throughput: 44.50%
Achieved Occupancy: 66.57%
Registers Per Thread: 40
L2 Hit Rate: 59.46%
Executed IPC: 0.96 inst/cycle
L1/TEX Hit Rate: 0%
Shared Memory Per Block: 34,816 bytes
Theoretical Occupancy: 66.67%
Warp Cycles Per Instruction: 30.39 cycles
DRAM Throughput: 14.60%
SM Busy: 26.29%


Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 832.589   834.82
BLOCK_WORK_SIZE [32, 16]
ITEM_WORK_SIZE [2, 4]

Memory Throughput: 78.84%
Compute (SM) Throughput: 77.70%
Achieved Occupancy: 63.48%
Registers Per Thread: 40
L2 Hit Rate: 70.59%
Executed IPC: 1.86 inst/cycle
L1/TEX Hit Rate: 0.10%
Shared Memory Per Block: 34,816 bytes
Theoretical Occupancy: 66.67%
Shared Memory Bank Conflicts: 18.43%
Warp Cycles Per Instruction: 15.04 cycles
DRAM Throughput: 19.63%
SM Busy: 50.68%

Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 739.588   741.77
BLOCK_WORK_SIZE [32, 4]
ITEM_WORK_SIZE [2, 16]

Memory Throughput: 58.98%
Compute (SM) Throughput: 57.94%
Achieved Occupancy: 16.50%
Registers Per Thread: 106
L2 Hit Rate: 68.50%
Executed IPC: 1.88 inst/cycle
L1/TEX Hit Rate: 17.09%
Shared Memory Per Block: 34,816 bytes
Theoretical Occupancy: 16.67%
Shared Memory Bank Conflicts: 17.31%
Warp Cycles Per Instruction: 3.86 cycles
DRAM Throughput: 22.89%
SM Busy: 51.31%

Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 983.222   985.455
BLOCK_WORK_SIZE [8, 2]
ITEM_WORK_SIZE [4, 16]

Memory Throughput: 48.65%
Compute (SM) Throughput: 61.12%
Achieved Occupancy: 19.07%
Registers Per Thread: 148
L2 Hit Rate: 80.48%
Executed IPC: 2.44 inst/cycle
L1/TEX Hit Rate: 12.45%
Shared Memory Per Block: 9,216 bytes
Theoretical Occupancy: 20.83%
Shared Memory Bank Conflicts: 40.03%
Warp Cycles Per Instruction: 3.62 cycles
DRAM Throughput: 17.04%
SM Busy: 62.95%

### Анализ метрик

Executed IPC принимает наибольшее занчение при BLOCK_WORK_SIZE [8, 2] ITEM_WORK_SIZE [4, 16], а также ITEM_WORK_SIZE [4, 4], что объясняется бОльшими VEC, RPI, по сравнению с другими вызвоами метода. В свою очередь это должно приводить к дополнительной нагрузке на регистры.

Заметим, что у 2 реализации в среднем намного лучше реализована работа с кэшами, по сравнению с первой, так как читаем смежные элементы векторно, предположу, что под капотом это поведение для процессора более предсказуемо, данные реже из кэша вытесняются, уменьшается число запросов к памяти.

Заметим, что при TILE_SIZE = 64 заметно меньше банковских конфликтов, чем при TILE_SIZE = 32 => warp эффективнее работает.

Achieved Occupancy сильно падает при росте числа регистров на поток, так как на SM можно разместить меньшее количество warps из-за ограниченного числа регистров. Кроме того, часть регистров может сброситься в локальную память, что увеличивает задержки доступа к данным и негативно влияет на время выполнения.

SM Busy и Warp Cycles Per Instruction зависят от эффективности использования кэшей и минимизации банковых конфликтов, но не коррелируют с временем исполнения.

Низкий DRAM Throughput говорит о том, что ядра берут данные из shared, кэшей, что хорошо.

Время работы на GPU определяется эффективностью использования регистров, shared memory и кэш-памяти, а также наличием банковских конфликтов, а DRAM Throughput, SM Busy, Memory Throughput, Compute Throughput и Achieved Occupancy, не всегда отражают реальность.

```c
template<int TILE_SIZE>
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
        for (int k = 0; k < TILE_SIZE; k++) {
            acc += tile_a[local_row][k] * tile_b[k][local_col];
        }

        __syncthreads();
    }

    C[c_row * N + c_col] = acc;
}
```

Version 1

Tile_size = 16
Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 1703.74   1705.89
BLOCK_WORK_SIZE [16, 16]
ITEM_WORK_SIZE [1, 1]

Memory Throughput: 98.32%
Compute (SM) Throughput: 98.32%
Achieved Occupancy: 98.30%
Registers Per Thread: 36
L2 Hit Rate: 40.85%
Executed IPC: 0.85 inst/cycle
L1/TEX Hit Rate: 15.42%
Shared Memory Per Block: 2,176 bytes
Theoretical Occupancy: 100%
Shared Memory Bank Conflicts: 50.36%
Warp Cycles Per Instruction: 55.20 cycles
DRAM Throughput: 19.83%
SM Busy: 34.70%

Tile_Size = 32
Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 1550.78   1553
BLOCK_WORK_SIZE [32, 32]
ITEM_WORK_SIZE [1, 1]

Memory Throughput: 87.69%
Compute (SM) Throughput: 87.69%
Achieved Occupancy: 66.67%
Registers Per Thread: 38
L2 Hit Rate: 47.77%
Executed IPC: 0.71 inst/cycle
L1/TEX Hit Rate: 0%
Shared Memory Per Block: 8,448 bytes
Theoretical Occupancy: 66.67%
Shared Memory Bank Conflicts: 19.48%
Warp Cycles Per Instruction: 44.21 cycles
DRAM Throughput: 9.55%
SM Busy: 30.50%

Device: NVIDIA GeForce RTX 3060 Version: 12020
Time: 1347.2    1349.64 (изменил   __shared__ float tile_b[TILE_SIZE][TILE_SIZE + 4];) было +1
BLOCK_WORK_SIZE [32, 32]
ITEM_WORK_SIZE [1, 1]

Memory Throughput 82.12%
Compute (SM) Throughput	82.12%
Achieved Occupancy 66.82%
Registers Per Thread 38
L2 Hit Rate	47.34%
Executed IPC 0.80 inst/cycle
L1/TEX Hit Rate	0%
Shared Memory Per Block	9,216 bytes
Theoretical Occupancy 66.67%
Shared Memory Bank Conflicts 19.45%
Warp Cycles Per Instruction	39.81 cycles
DRAM Throughput	14.03%
SM Busy	29.10%

Видна просадка по кэшам при TILE_SIZE = 32, тк больший размер матрицы задействуем.
Видно, что при 32 меньше конфликтов банка. 

Высокие проценты Memory/Compute Throughput не гарантируют оптимальное время. Важнее отсутсвие bottleneck (конфилкты банков, registers spilling), что было контринтуитивным для меня.
При Registers per Thread > 100 падает время из-за вытеснения регистров.
При увеличении Shared Bank Conflicts время ожидаемо ухудшается.

## Вывод по гиперпарамтерам 

- Реализация 0: `TILE_SIZE = 32`.
- Реализация 1: `TILE_SIZE = 32`.
- Реализация 2: оптимальные результаты дают конфигурации `BLOCK_WORK_SIZE [32, 4]`, `ITEM_WORK_SIZE [2, 16]` (или `BLOCK [16,16] / ITEM [4,4]` на вашем сервере).

Замеры на графиках можно посмотреть по [chart_kernels.png](charts/chart_kernels.png).
