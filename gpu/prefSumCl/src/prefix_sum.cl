#if (VEC == 4)
typedef uint4 vec_t;
#define VLOAD(ptr) vload4(0, (ptr))
#define VSTORE(v, p) vstore4((v), 0, (p))
#elif (VEC == 8)
typedef uint8 vec_t;
#define VLOAD(ptr) vload8(0, (ptr))
#define VSTORE(v, p) vstore8((v), 0, (p))
#elif (VEC == 16)
typedef uint16 vec_t;
#define VLOAD(ptr) vload16(0, (ptr))
#define VSTORE(v, p) vstore16((v), 0, (p))
#endif

__kernel void block_scan(__global const uint* __restrict src, __global uint* __restrict dst, __global uint* __restrict block_sums) {
    __local uint tile[TILE];

    const uint local_id = get_local_id(0);
    const uint group_id = get_group_id(0);
    const uint idx = group_id * TILE + local_id;

    tile[local_id] = src[idx];
    barrier(CLK_LOCAL_MEM_FENCE);

#pragma unroll
    for (uint offset = 1; offset < TILE; offset <<= 1) {
        const uint v = (local_id >= offset) ? tile[local_id - offset] : 0;
        barrier(CLK_LOCAL_MEM_FENCE);
        tile[local_id] += v;
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    dst[idx] = tile[local_id];
    if (local_id == TILE - 1) block_sums[group_id] = tile[local_id];
}

__kernel void brent_kung(__global uint* __restrict buf) {
    __local uint s[TILE];
    const uint local_id = get_local_id(0);

    s[local_id] = buf[local_id];
    barrier(CLK_LOCAL_MEM_FENCE);

#pragma unroll
    for (uint offset = 1; offset < TILE; offset <<= 1) {
        const uint idx = (local_id + 1) * (2 * offset) - 1;
        if (idx < TILE) s[idx] += s[idx - offset];
        barrier(CLK_LOCAL_MEM_FENCE);
    }

#pragma unroll
    for (uint offset = TILE >> 1; offset > 0; offset >>= 1) {
        const uint idx = (local_id + 1) * (2 * offset) - 1;
        if (idx + offset < TILE) s[idx + offset] += s[idx];
        barrier(CLK_LOCAL_MEM_FENCE);
    }

    buf[local_id] = s[local_id];
}

__kernel void add_offsets(__global uint* __restrict dst, __global const uint* __restrict block_prefix, const uint n) {
    const uint group_id = get_group_id(0);
    const uint local_id = get_local_id(0);

    const uint base_idx = group_id * TILE + local_id * VEC;
    uint offset = (group_id == 0) ? 0 : block_prefix[group_id - 1];
    vec_t data = VLOAD(dst + base_idx);
    data += (vec_t)(offset);
    VSTORE(data, dst + base_idx);
}
