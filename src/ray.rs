mod vec3;
use vec3::*;

pub struct Ray {
  pub orig: Vec3,
  pub dir: Vec3,
}

impl Ray {
  pub fn at(self, t: f64) -> Vec3 {
    orig + t * dir;
  }
}
