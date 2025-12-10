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
        const Symbol& c0_0 = correlations[0][0][shift0];
        const Symbol& c0_1 = correlations[0][1][shift0];
        const Symbol& c0_2 = correlations[0][2][shift0];
        const Symbol& c0_3 = correlations[0][3][shift0];

        const Symbol& c1_0 = correlations[1][0][shift1];
        const Symbol& c1_1 = correlations[1][1][shift1];
        const Symbol& c1_2 = correlations[1][2][shift1];
        const Symbol& c1_3 = correlations[1][3][shift1];

        const float r0_0 = c0_0.real(), i0_0 = c0_0.imag();
        const float r0_1 = c0_1.real(), i0_1 = c0_1.imag();
        const float r0_2 = c0_2.real(), i0_2 = c0_2.imag();
        const float r0_3 = c0_3.real(), i0_3 = c0_3.imag();

        const float r1_0 = c1_0.real(), i1_0 = c1_0.imag();
        const float r1_1 = c1_1.real(), i1_1 = c1_1.imag();
        const float r1_2 = c1_2.real(), i1_2 = c1_2.imag();
        const float r1_3 = c1_3.real(), i1_3 = c1_3.imag();

        __m128 re0 = _mm_setr_ps(r0_0, r0_1, r0_2, r0_3);
        __m128 im0 = _mm_setr_ps(i0_0, i0_1, i0_2, i0_3);
        __m128 re1 = _mm_setr_ps(r1_0, r1_1, r1_2, r1_3);
        __m128 im1 = _mm_setr_ps(i1_0, i1_1, i1_2, i1_3);

        __m128 norm0 = _mm_add_ps(_mm_mul_ps(re0, re0), _mm_mul_ps(im0, im0));
        __m128 norm1 = _mm_add_ps(_mm_mul_ps(re1, re1), _mm_mul_ps(im1, im1));

        __m128 sum    = _mm_add_ps(norm0, norm1);
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
