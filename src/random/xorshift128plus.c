/*  Written in 2014-2016 by Sebastiano Vigna (vigna@acm.org)

To the extent possible under law, the author has dedicated all copyright
and related and neighboring rights to this software to the public domain
worldwide. This software is distributed without any warranty.

See <http://creativecommons.org/publicdomain/zero/1.0/>. */

#include <stdint.h>

/* This generator has been replaced by xoroshiro128plus, which is
   significantly faster and has better statistical properties.

   It might be nonetheless useful for languages in which low-level rotate
   instructions are not available. Due to the relatively short period it
   is acceptable only for applications with a mild amount of parallelism;
   otherwise, use a xorshift1024* generator.

   Note that the lowest bit of this generator is an LSFR, and thus it will
   fail binary rank tests. The next bit is not an LFSR, but in the long
   run it will fail binary rank tests, too. The other bits have no LFSR
   artifacts.

   We suggest to use a sign test to extract a random Boolean value, and
   right shifts to extract subsets of bits.

   The state must be seeded so that it is not everywhere zero. If you have
   a 64-bit seed, we suggest to seed a splitmix64 generator and use its
   output to fill s.

   A previous version of this generator was adding the two halves of the
   newly computed state. This version adds the two halves of the *current*
   state (as xoroshiro128plus does), which improves speed due to better
   internal parallelization from the CPU. The resulting streams are off by
   one step. */

uint64_t s[2];

uint64_t next(void) {
    uint64_t s1 = s[0];
    const uint64_t s0 = s[1];
    const uint64_t result = s0 + s1;
    s[0] = s0;
    s1 ^= s1 << 23; // a
    s[1] = s1 ^ s0 ^ (s1 >> 18) ^ (s0 >> 5); // b, c
    return result; 
}


/* This is the jump function for the generator. It is equivalent
   to 2^64 calls to next(); it can be used to generate 2^64
   non-overlapping subsequences for parallel computations. */

void jump(void) {
    static const uint64_t JUMP[] = { 0x8a5cd789635d2dff, 0x121fd2155c472f96 };

    uint64_t s0 = 0;
    uint64_t s1 = 0;
    for(int i = 0; i < sizeof JUMP / sizeof *JUMP; i++)
        for(int b = 0; b < 64; b++) {
            if (JUMP[i] & UINT64_C(1) << b) {
                s0 ^= s[0];
                s1 ^= s[1];
            }
            next();
        }

    s[0] = s0;
    s[1] = s1;
}
