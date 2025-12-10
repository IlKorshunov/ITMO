#include "PucchF3EstimateCorrPower.h"
#include <immintrin.h>
#include <complex>

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
    constexpr float invNumOfDmrsSymbols = 1.0f / numOfDmrsSymbols;
    
    for (uint32_t antIdx = 0; antIdx < numOfAnts; ++antIdx)
    {
        // Векторизованное вычисление суммы норм
        __m256 sum_vec = _mm256_setzero_ps();
        uint32_t symbIdx = 0;
        
        // Обрабатываем по 4 комплексных числа за раз (8 float значений)
        // AVX может обработать 8 float одновременно
        for (; symbIdx + 4 <= numOfDmrsSymbols; symbIdx += 4)
        {
            // Собираем 4 комплексных числа в правильном порядке
            // Layout: [re0, im0, re1, im1, re2, im2, re3, im3]
            alignas(32) float re_im_data[8];
            for (uint32_t i = 0; i < 4; ++i)
            {
                const auto& c = correlations[symbIdx + i][antIdx][dmrsCyclicShifts[symbIdx + i]];
                re_im_data[i * 2] = c.real();
                re_im_data[i * 2 + 1] = c.imag();
            }
            
            __m256 re_im = _mm256_load_ps(re_im_data);
            
            // Извлекаем real и imaginary части
            // re_im = [re0, im0, re1, im1, re2, im2, re3, im3]
            // Используем shuffle для извлечения:
            // re = [re0, re1, re2, re3, re0, re1, re2, re3] (используем только первые 4)
            // im = [im0, im1, im2, im3, im0, im1, im2, im3] (используем только первые 4)
            __m256 re = _mm256_shuffle_ps(re_im, re_im, _MM_SHUFFLE(2, 0, 2, 0));
            __m256 im = _mm256_shuffle_ps(re_im, re_im, _MM_SHUFFLE(3, 1, 3, 1));
            
            // Вычисляем норму: norm = re*re + im*im
            // Используем FMA если доступно: norm = fma(re, re, im*im)
            // Иначе: norm = re*re + im*im
#ifdef __FMA__
            __m256 re_sq = _mm256_mul_ps(re, re);
            __m256 norm = _mm256_fmadd_ps(im, im, re_sq);
#else
            __m256 re_sq = _mm256_mul_ps(re, re);
            __m256 im_sq = _mm256_mul_ps(im, im);
            __m256 norm = _mm256_add_ps(re_sq, im_sq);
#endif
            
            // Добавляем к аккумулятору суммы
            sum_vec = _mm256_add_ps(sum_vec, norm);
        }
        
        // Обрабатываем оставшиеся элементы скалярно
        float scalar_sum = 0.0f;
        for (; symbIdx < numOfDmrsSymbols; ++symbIdx)
        {
            scalar_sum += std::norm(correlations[symbIdx][antIdx][dmrsCyclicShifts[symbIdx]]);
        }
        
        // Горизонтальное сложение для векторизованной суммы
        // Извлекаем верхнюю и нижнюю половины 256-битного регистра
        __m128 sum_low = _mm256_extractf128_ps(sum_vec, 0);
        __m128 sum_high = _mm256_extractf128_ps(sum_vec, 1);
        __m128 sum_128 = _mm_add_ps(sum_low, sum_high);
        
        // Горизонтальное сложение: hadd(a,b) = [a0+a1, a2+a3, b0+b1, b2+b3]
        sum_128 = _mm_hadd_ps(sum_128, sum_128);
        sum_128 = _mm_hadd_ps(sum_128, sum_128);
        
        float vectorized_sum = _mm_cvtss_f32(sum_128);
        
        // Итоговая сумма и нормализация
        corrPower[antIdx] = (vectorized_sum + scalar_sum) * invNumOfDmrsSymbols;
    }
}

}  // namespace rt::ul::ttiProcessor::pucch::format3
