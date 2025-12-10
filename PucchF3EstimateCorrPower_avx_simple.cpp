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
        const Symbol* c0 = &correlations[0][0][shift0];
        const Symbol* c1 = &correlations[1][0][shift1];

        const float* c0_data = reinterpret_cast<const float*>(c0);
        const float* c1_data = reinterpret_cast<const float*>(c1);

        __m256 c0_vec = _mm256_loadu_ps(c0_data);
        __m256 c1_vec = _mm256_loadu_ps(c1_data);

        __m256 c0_lo = _mm256_unpacklo_ps(c0_vec, c0_vec);
        __m256 c0_hi = _mm256_unpackhi_ps(c0_vec, c0_vec);
        __m256 c0_real = _mm256_shuffle_ps(c0_lo, c0_hi, _MM_SHUFFLE(2, 0, 2, 0));
        __m256 c0_imag = _mm256_shuffle_ps(c0_lo, c0_hi, _MM_SHUFFLE(3, 1, 3, 1));

        __m256 c1_lo = _mm256_unpacklo_ps(c1_vec, c1_vec);
        __m256 c1_hi = _mm256_unpackhi_ps(c1_vec, c1_vec);
        __m256 c1_real = _mm256_shuffle_ps(c1_lo, c1_hi, _MM_SHUFFLE(2, 0, 2, 0));
        __m256 c1_imag = _mm256_shuffle_ps(c1_lo, c1_hi, _MM_SHUFFLE(3, 1, 3, 1));

        __m256 norm0 = _mm256_fmadd_ps(c0_real, c0_real, _mm256_mul_ps(c0_imag, c0_imag));
        __m256 norm1 = _mm256_fmadd_ps(c1_real, c1_real, _mm256_mul_ps(c1_imag, c1_imag));

        __m256 sum = _mm256_add_ps(norm0, norm1);
        __m256 result = _mm256_mul_ps(sum, _mm256_set1_ps(invNumOfDmrsSymbols));

        _mm256_storeu_ps(corrPower, result);
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
