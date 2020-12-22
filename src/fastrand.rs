// https://lemire.me/blog/2019/03/19/the-fastest-conventional-random-number-generator-that-can-pass-big-crush/
// https://experilous.com/1/blog/post/perfect-fast-random-floating-point-numbers

static mut STATE: u128 = 1232451235;

// fn rand_u64() -> u64 {
//   unsafe {
//     STATE = STATE * 0xda942042e4dd58b5;
//     (STATE >> 64) as u64
//   }
// }

fn rand_u32() -> u32 {
  unsafe {
    STATE = STATE * 0xda942042e4dd58b5;
    // is this correct? :)
    (STATE >> 96) as u32
  }
}

pub fn rand_f32_01() -> f32 {
  f32::from_bits(0x3F800000 | (rand_u32() >> 9)) - 1.0
}

pub fn rand_f32_interval(min: f32, max: f32) -> f32 {
  // a little questionable if this makes a good random number in the interval... but it looks ok!
  min + (max - min) * rand_f32_01()
}
