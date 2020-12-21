use crate::ray::*;
use crate::vec3::*;

pub struct Hit {
  pub p: Vec3,
  pub norm: Vec3,
  pub t: f64,
  pub front_face: bool,
}

impl Hit {
  // pub fn set_face_normal(mut self, ray: &Ray, outward_normal: Vec3) {
  //   self.front_face = dot(ray.dir, outward_normal) < 0f64;
  //   self.norm = if self.front_face { outward_normal } else { - outward_normal };
  // }

  pub fn with_face_normal(p: Vec3, t: f64, ray: &Ray, outward_normal: Vec3) -> Self {
    let front_face = dot(ray.dir, outward_normal) < 0f64;
    let norm = if front_face {
      outward_normal
    } else {
      -outward_normal
    };

    Hit {
      p,
      norm,
      t,
      front_face,
    }
  }
}

pub trait Hittable {
  fn hit(&self, ray: &Ray, tmin: f64, tmax: f64) -> Option<Hit>;
}
