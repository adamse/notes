use crate::util::*;

use rand::{thread_rng, Rng};
use std::fmt;
use std::ops;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Vec3 {
  pub x: f32,
  pub y: f32,
  pub z: f32,
}

pub fn colour(x: f32, y: f32, z: f32) -> Vec3 {
  Vec3 { x, y, z }
}
pub fn point(x: f32, y: f32, z: f32) -> Vec3 {
  Vec3 { x, y, z }
}
pub fn vec(x: f32, y: f32, z: f32) -> Vec3 {
  Vec3 { x, y, z }
}

pub fn dot(u: Vec3, v: Vec3) -> f32 {
  u.x * v.x + u.y * v.y + u.z * v.z
}

impl Vec3 {
  pub fn new(x: f32, y: f32, z: f32) -> Self {
    Vec3 { x, y, z }
  }
  pub fn zero() -> Self {
    Vec3 {
      x: 0.0,
      y: 0.0,
      z: 0.0,
    }
  }
  pub fn one() -> Self {
    Vec3 {
      x: 1.0,
      y: 1.0,
      z: 1.0,
    }
  }
  pub fn length(self) -> f32 {
    self.length_squared().sqrt()
  }
  pub fn length_squared(self) -> f32 {
    dot(self, self)
  }
  pub fn cross(u: &Self, v: &Self) -> Self {
    Vec3 {
      x: u.y * v.z - u.z * v.y,
      y: u.z * v.x - u.x * v.z,
      z: u.x * v.y - u.y * v.x,
    }
  }
  pub fn unit(self) -> Self {
    self / self.length()
  }

  pub fn colour_fmt(&self, samples_per_pixel: u32) -> String {
    let scale = 1.0 / samples_per_pixel as f32;
    let colour = |i: f32| -> u32 { (256.0 * clamp(i * scale, 0.0, 0.999)) as u32 };
    format!("{} {} {}", colour(self.x), colour(self.y), colour(self.z))
  }

  pub fn random_unit() -> Self {
    Self::random(0.0, 1.0)
  }

  pub fn random(min: f32, max: f32) -> Self {
    let mut rng = thread_rng();
    Vec3 {
      x: rng.gen_range(min..max),
      y: rng.gen_range(min..max),
      z: rng.gen_range(min..max),
    }
  }

  pub fn random_unit_sphere() -> Self {
    loop {
      let p = Self::random(-1.0, 1.0);
      if p.length_squared() < 1.0 {
        return p;
      }
    }
  }
}

impl fmt::Display for Vec3 {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({}, {}, {})", self.x, self.y, self.z)
  }
}

impl ops::Add for Vec3 {
  type Output = Self;

  fn add(self, o: Self) -> Self {
    Vec3 {
      x: self.x + o.x,
      y: self.y + o.y,
      z: self.z + o.z,
    }
  }
}

impl ops::AddAssign for Vec3 {
  fn add_assign(&mut self, o: Self) {
    self.x += o.x;
    self.y += o.y;
    self.z += o.z;
  }
}

impl ops::Sub for Vec3 {
  type Output = Self;

  fn sub(self, o: Self) -> Self {
    Vec3 {
      x: self.x - o.x,
      y: self.y - o.y,
      z: self.z - o.z,
    }
  }
}

impl ops::Neg for Vec3 {
  type Output = Self;

  fn neg(self) -> Self {
    Vec3 {
      x: -self.x,
      y: -self.y,
      z: -self.z,
    }
  }
}

impl ops::Mul<Vec3> for f32 {
  type Output = Vec3;
  fn mul(self, o: Vec3) -> Vec3 {
    o * self
  }
}
impl ops::Mul<f32> for Vec3 {
  type Output = Self;
  fn mul(self, o: f32) -> Self {
    Vec3 {
      x: self.x * o,
      y: self.y * o,
      z: self.z * o,
    }
  }
}

impl ops::MulAssign<f32> for Vec3 {
  fn mul_assign(&mut self, o: f32) {
    self.x *= o;
    self.y *= o;
    self.z *= o;
  }
}

impl ops::Div<Vec3> for f32 {
  type Output = Vec3;
  fn div(self, o: Vec3) -> Vec3 {
    o / self
  }
}
impl ops::Div<f32> for Vec3 {
  type Output = Self;
  fn div(self, o: f32) -> Self {
    Vec3 {
      x: self.x / o,
      y: self.y / o,
      z: self.z / o,
    }
  }
}

impl ops::DivAssign<f32> for Vec3 {
  fn div_assign(&mut self, o: f32) {
    self.x /= o;
    self.y /= o;
    self.z /= o;
  }
}
