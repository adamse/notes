use crate::vec3::*;

pub struct Ray {
  pub orig: Vec3,
  pub dir: Vec3,
}

pub fn ray(orig: Vec3, dir: Vec3) -> Ray {
  Ray { orig, dir }
}

impl Ray {
  pub fn at(&self, t: f32) -> Vec3 {
    self.orig + t * self.dir
  }
}
