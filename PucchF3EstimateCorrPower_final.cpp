#include "PucchF3EstimateCorrPower.h"
#include <immintrin.h>

namespace rt::ul::ttiProcessor::pucch::format3
{
using namespace constants;
using namespace btsConstants;
using common::ul::pucch::Symbol;

void estimateCorrPower(const Symbol (&correlations)[numOfDmrsSymbols][maxUlAntennaPortNum],
                       const uint32_t          numOfAnts,
                       const DmrsCyclicShifts& dmrsCyclicShifts,
                       float (&corrPower)[maxUlAntennaPortNum])
{
    const uint8_t shift0 = dmrsCyclicShifts[0];
    const uint8_t shift1 = dmrsCyclicShifts[1];
    constexpr float invNumOfDmrsSymbols = 0.5f; // 1.0f / 2.0f (2 DMRS символа всегда)

    if (numOfAnts == 1)
    {
        // Оптимизация: прямое вычисление вместо std::norm() - избегаем overhead вызова функции
        const Symbol& c0 = correlations[0][0][shift0];
        const Symbol& c1 = correlations[1][0][shift1];
        const float r0 = c0.real(), i0 = c0.imag();
        const float r1 = c1.real(), i1 = c1.imag();
        corrPower[0] = (r0 * r0 + i0 * i0 + r1 * r1 + i1 * i1) * invNumOfDmrsSymbols;
        return;
    }
    else if (numOfAnts == 2)
    {
        // Оптимизация: SIMD для 2 антенн - лучше чем цикл для малого числа элементов
        const Symbol& c0_0 = correlations[0][0][shift0];
        const Symbol& c0_1 = correlations[0][1][shift0];
        const Symbol& c1_0 = correlations[1][0][shift1];
        const Symbol& c1_1 = correlations[1][1][shift1];

        // Загружаем данные: используем _mm_setr_ps для естественного порядка [0, 1, 0, 0]
        // Последние два элемента не используются, но это дешево
        __m128 re0 = _mm_setr_ps(c0_0.real(), c0_1.real(), 0.0f, 0.0f);
        __m128 im0 = _mm_setr_ps(c0_0.imag(), c0_1.imag(), 0.0f, 0.0f);
        __m128 re1 = _mm_setr_ps(c1_0.real(), c1_1.real(), 0.0f, 0.0f);
        __m128 im1 = _mm_setr_ps(c1_0.imag(), c1_1.imag(), 0.0f, 0.0f);

        // Вычисляем нормы: |z|^2 = re^2 + im^2
        __m128 norm0 = _mm_add_ps(_mm_mul_ps(re0, re0), _mm_mul_ps(im0, im0));
        __m128 norm1 = _mm_add_ps(_mm_mul_ps(re1, re1), _mm_mul_ps(im1, im1));

        // Суммируем и умножаем на 0.5
        __m128 sum    = _mm_add_ps(norm0, norm1);
        __m128 result = _mm_mul_ps(sum, _mm_set1_ps(invNumOfDmrsSymbols));

        // Сохраняем результат (только первые 2 элемента используются)
        alignas(16) float temp[4];
        _mm_store_ps(temp, result);
        corrPower[0] = temp[0];
        corrPower[1] = temp[1];
        return;
    }
    else if (numOfAnts == 4)
    {
        // Оптимизация: более эффективная загрузка с _mm_setr_ps
        // _mm_setr_ps загружает в естественном порядке [a, b, c, d] -> [a, b, c, d]
        // _mm_set_ps загружает в обратном порядке [a, b, c, d] -> [d, c, b, a]
        const Symbol& c0_0 = correlations[0][0][shift0];
        const Symbol& c0_1 = correlations[0][1][shift0];
        const Symbol& c0_2 = correlations[0][2][shift0];
        const Symbol& c0_3 = correlations[0][3][shift0];

        const Symbol& c1_0 = correlations[1][0][shift1];
        const Symbol& c1_1 = correlations[1][1][shift1];
        const Symbol& c1_2 = correlations[1][2][shift1];
        const Symbol& c1_3 = correlations[1][3][shift1];

        // Используем _mm_setr_ps для более естественного порядка загрузки
        // Это генерирует тот же код, но логически проще для понимания
        __m128 re0 = _mm_setr_ps(c0_0.real(), c0_1.real(), c0_2.real(), c0_3.real());
        __m128 im0 = _mm_setr_ps(c0_0.imag(), c0_1.imag(), c0_2.imag(), c0_3.imag());

        __m128 re1 = _mm_setr_ps(c1_0.real(), c1_1.real(), c1_2.real(), c1_3.real());
        __m128 im1 = _mm_setr_ps(c1_0.imag(), c1_1.imag(), c1_2.imag(), c1_3.imag());

        // Вычисляем нормы
        __m128 norm0 = _mm_add_ps(_mm_mul_ps(re0, re0), _mm_mul_ps(im0, im0));
        __m128 norm1 = _mm_add_ps(_mm_mul_ps(re1, re1), _mm_mul_ps(im1, im1));

        // Суммируем и умножаем на 0.5
        __m128 sum    = _mm_add_ps(norm0, norm1);
        __m128 result = _mm_mul_ps(sum, _mm_set1_ps(invNumOfDmrsSymbols));

        _mm_storeu_ps(corrPower, result);
        return;
    }
    else
    {
        // Fallback: прямое вычисление без std::norm() для лучшей производительности
        for (uint32_t antIdx = 0; antIdx < numOfAnts; ++antIdx)
        {
            const Symbol& c0 = correlations[0][antIdx][shift0];
            const Symbol& c1 = correlations[1][antIdx][shift1];
            const float r0 = c0.real(), i0 = c0.imag();
            const float r1 = c1.real(), i1 = c1.imag();
            corrPower[antIdx] = (r0 * r0 + i0 * i0 + r1 * r1 + i1 * i1) * invNumOfDmrsSymbols;
        }
    }
}
