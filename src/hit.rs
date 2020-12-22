use crate::material::*;
use crate::ray::*;
use crate::vec3::*;

pub struct Hit {
  pub p: Vec3,
  pub norm: Vec3,
  pub t: f32,
  pub front_face: bool,
  pub material: Material,
}

impl Hit {
  // pub fn set_face_normal(mut self, ray: &Ray, outward_normal: Vec3) {
  //   self.front_face = dot(ray.dir, outward_normal) < 0f32;
  //   self.norm = if self.front_face { outward_normal } else { - outward_normal };
  // }

  pub fn face_normal(
    p: Vec3,
    t: f32,
    ray: &Ray,
    outward_normal: Vec3,
    material: Material,
  ) -> Self {
    let front_face = dot(ray.dir, outward_normal) < 0f32;
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
      material,
    }
  }
}

pub trait Hittable {
  fn hit(&self, ray: &Ray, tmin: f32, tmax: f32) -> Option<Hit>;
}
