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

        __m128 re0 = _mm_setr_ps(c0[0].real(), c0[1].real(), c0[2].real(), c0[3].real());
        __m128 im0 = _mm_setr_ps(c0[0].imag(), c0[1].imag(), c0[2].imag(), c0[3].imag());
        __m128 re1 = _mm_setr_ps(c1[0].real(), c1[1].real(), c1[2].real(), c1[3].real());
        __m128 im1 = _mm_setr_ps(c1[0].imag(), c1[1].imag(), c1[2].imag(), c1[3].imag());

        __m128 re0_sq = _mm_mul_ps(re0, re0);
        __m128 im0_sq = _mm_mul_ps(im0, im0);
        __m128 re1_sq = _mm_mul_ps(re1, re1);
        __m128 im1_sq = _mm_mul_ps(im1, im1);

        __m128 norm0 = _mm_add_ps(re0_sq, im0_sq);
        __m128 norm1 = _mm_add_ps(re1_sq, im1_sq);

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
