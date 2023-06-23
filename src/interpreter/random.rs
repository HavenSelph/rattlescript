/*
    Written in 2019 by David Blackman and Sebastiano Vigna (vigna@acm.org)

    To the extent possible under law, the author has dedicated all copyright
    and related and neighboring rights to this software to the public domain
    worldwide. This software is distributed without any warranty.

    See <http://creativecommons.org/publicdomain/zero/1.0/>.

    This is xoshiro256++ 1.0, one of our all-purpose, rock-solid generators.
    It has excellent (sub-ns) speed, a state (256 bits) that is large
    enough for any parallel application, and it passes all tests we are
    aware of.

    For generating just floating-point numbers, xoshiro256+ is even faster.

    The state must be seeded so that it is not everywhere zero. If you have
    a 64-bit seed, we suggest to seed a splitmix64 generator and use its
    output to fill s.
*/

use std::time::{SystemTime, UNIX_EPOCH};

pub struct RandomState {
    s0: u64,
    s1: u64,
    s2: u64,
    s3: u64,
}

impl RandomState {
    pub fn new() -> Self {
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
        // Turn time's last 4 digits into a f64
        let time_: u64 = (time % 10000).rotate_right(4);
        let mut s = RandomState {
            s0: time << (time_ % 16),
            s1: 0,
            s2: time_,
            s3: 0,
        };
        for _ in 0..(time % 10) {
            s.next();
        };
        s.s0 = s.next();
        s.s3 = s.next();
        s
    }

    pub fn next(&mut self) -> u64 {
        let result = (self.s0.overflowing_add(self.s3).0).rotate_left(23).overflowing_add(self.s0).0;
        let t = self.s1 << 17;

        self.s2 ^= self.s0;
        self.s3 ^= self.s1;
        self.s1 ^= self.s2;
        self.s0 ^= self.s3;

        self.s2 ^= t;
        self.s3 = self.s3.rotate_left(45);
        result
    }
}
