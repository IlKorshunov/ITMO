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
    constexpr float invNumOfDmrsSymbols = 0.5f; // 1.0f / 2.0f

    if (numOfAnts == 1)
    {
        // Оптимизация: прямое вычисление вместо std::norm()
        const Symbol& c0 = correlations[0][0][shift0];
        const Symbol& c1 = correlations[1][0][shift1];
        const float norm0 = c0.real() * c0.real() + c0.imag() * c0.imag();
        const float norm1 = c1.real() * c1.real() + c1.imag() * c1.imag();
        corrPower[0] = (norm0 + norm1) * invNumOfDmrsSymbols;
        return;
    }
    else if (numOfAnts == 2)
    {
        // Оптимизация: используем SIMD для 2 антенн
        const Symbol& c0_0 = correlations[0][0][shift0];
        const Symbol& c0_1 = correlations[0][1][shift0];
        const Symbol& c1_0 = correlations[1][0][shift1];
        const Symbol& c1_1 = correlations[1][1][shift1];

        // Загружаем real и imag части для двух антенн
        // Используем _mm_set_ps с нулями для последних двух элементов
        __m128 re0 = _mm_set_ps(0.0f, 0.0f, c0_1.real(), c0_0.real());
        __m128 im0 = _mm_set_ps(0.0f, 0.0f, c0_1.imag(), c0_0.imag());
        __m128 re1 = _mm_set_ps(0.0f, 0.0f, c1_1.real(), c1_0.real());
        __m128 im1 = _mm_set_ps(0.0f, 0.0f, c1_1.imag(), c1_0.imag());

        __m128 norm0 = _mm_add_ps(_mm_mul_ps(re0, re0), _mm_mul_ps(im0, im0));
        __m128 norm1 = _mm_add_ps(_mm_mul_ps(re1, re1), _mm_mul_ps(im1, im1));

        __m128 sum    = _mm_add_ps(norm0, norm1);
        __m128 result = _mm_mul_ps(sum, _mm_set1_ps(invNumOfDmrsSymbols));

        // Сохраняем только первые 2 элемента
        alignas(16) float temp[4];
        _mm_store_ps(temp, result);
        corrPower[0] = temp[0];
        corrPower[1] = temp[1];
        return;
    }
    else if (numOfAnts == 4)
    {
        // Оптимизация: более эффективная загрузка данных
        // Вместо _mm_set_ps используем загрузку напрямую из памяти, если возможно
        // или используем более эффективную последовательность инструкций
        
        const Symbol& c0_0 = correlations[0][0][shift0];
        const Symbol& c0_1 = correlations[0][1][shift0];
        const Symbol& c0_2 = correlations[0][2][shift0];
        const Symbol& c0_3 = correlations[0][3][shift0];

        const Symbol& c1_0 = correlations[1][0][shift1];
        const Symbol& c1_1 = correlations[1][1][shift1];
        const Symbol& c1_2 = correlations[1][2][shift1];
        const Symbol& c1_3 = correlations[1][3][shift1];

        // Более эффективная загрузка: используем _mm_setr_ps для естественного порядка
        // или лучше - загружаем напрямую, если данные в памяти последовательно
        // Но так как у нас отдельные переменные, используем оптимизированную версию
        
        // Альтернатива: использовать _mm_setr_ps (reverse order) для более естественного порядка
        // или комбинировать загрузки более эффективно
        
        // Загружаем real части
        __m128 re0 = _mm_setr_ps(c0_0.real(), c0_1.real(), c0_2.real(), c0_3.real());
        __m128 im0 = _mm_setr_ps(c0_0.imag(), c0_1.imag(), c0_2.imag(), c0_3.imag());

        __m128 re1 = _mm_setr_ps(c1_0.real(), c1_1.real(), c1_2.real(), c1_3.real());
        __m128 im1 = _mm_setr_ps(c1_0.imag(), c1_1.imag(), c1_2.imag(), c1_3.imag());

        __m128 norm0 = _mm_add_ps(_mm_mul_ps(re0, re0), _mm_mul_ps(im0, im0));
        __m128 norm1 = _mm_add_ps(_mm_mul_ps(re1, re1), _mm_mul_ps(im1, im1));

        __m128 sum    = _mm_add_ps(norm0, norm1);
        __m128 result = _mm_mul_ps(sum, _mm_set1_ps(invNumOfDmrsSymbols));

        _mm_storeu_ps(corrPower, result);
        return;
    }
    else
    {
        // Fallback для других случаев (хотя по условию их не должно быть)
        for (uint32_t antIdx = 0; antIdx < numOfAnts; ++antIdx)
        {
            const Symbol& c0 = correlations[0][antIdx][shift0];
            const Symbol& c1 = correlations[1][antIdx][shift1];
            const float norm0 = c0.real() * c0.real() + c0.imag() * c0.imag();
            const float norm1 = c1.real() * c1.real() + c1.imag() * c1.imag();
            corrPower[antIdx] = (norm0 + norm1) * invNumOfDmrsSymbols;
        }
    }
}
