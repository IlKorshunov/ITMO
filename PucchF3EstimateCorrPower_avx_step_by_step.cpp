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
    constexpr float invNumOfDmrsSymbols = 0.5f;

    if (numOfAnts == 1)
    {
        const Symbol& c0 = correlations[0][0][shift0];
        const Symbol& c1 = correlations[1][0][shift1];
        const float r0 = c0.real(), i0 = c0.imag();
        const float r1 = c1.real(), i1 = c1.imag();
        corrPower[0] = (r0 * r0 + i0 * i0 + r1 * r1 + i1 * i1) * invNumOfDmrsSymbols;
        return;
    }
    else if (numOfAnts == 4)
    {
        const float* data0 = reinterpret_cast<const float*>(&correlations[0][0][shift0]);
        const float* data1 = reinterpret_cast<const float*>(&correlations[1][0][shift1]);

        __m256 all0 = _mm256_loadu_ps(data0);
        __m256 all1 = _mm256_loadu_ps(data1);

        __m256 sq0 = _mm256_mul_ps(all0, all0);
        __m256 sq1 = _mm256_mul_ps(all1, all1);

        __m256i permute_mask = _mm256_setr_epi32(0, 2, 4, 6, 1, 3, 5, 7);
        __m256 sq0_perm = _mm256_permutevar8x32_ps(sq0, permute_mask);
        __m256 sq1_perm = _mm256_permutevar8x32_ps(sq1, permute_mask);

        __m128 sq0_lo = _mm256_extractf128_ps(sq0_perm, 0);
        __m128 sq0_hi = _mm256_extractf128_ps(sq0_perm, 1);
        __m128 sq1_lo = _mm256_extractf128_ps(sq1_perm, 0);
        __m128 sq1_hi = _mm256_extractf128_ps(sq1_perm, 1);

        __m128 norm0 = _mm_add_ps(sq0_lo, sq0_hi);
        __m128 norm1 = _mm_add_ps(sq1_lo, sq1_hi);

        __m128 sum = _mm_add_ps(norm0, norm1);
        __m128 result = _mm_mul_ps(sum, _mm_set1_ps(invNumOfDmrsSymbols));

        _mm_storeu_ps(corrPower, result);
        return;
    }
    else
    {
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
