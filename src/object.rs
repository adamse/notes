use crate::hit::*;
use crate::material::*;
use crate::ray::*;
use crate::vec3::*;

pub struct Sphere<'a> {
  pub center: Vec3,
  pub radius: f32,
  pub material: &'a Material,
}

impl<'a> Hittable for Sphere<'a> {
  fn hit(&self, ray: &Ray, tmin: f32, tmax: f32) -> Option<Hit> {
    let oc = ray.orig - self.center;

    let a = dot(ray.dir, ray.dir);
    let half_b = dot(oc, ray.dir);
    let c = dot(oc, oc) - self.radius * self.radius;

    let discriminant = half_b * half_b - a * c;

    if discriminant < 0.0 {
      return None;
    }

    let sqrtd = discriminant.sqrt();

    // find the nearest root on the acceptible range
    let mut root = (-half_b - sqrtd) / a;
    if root < tmin || tmax < root {
      root = (-half_b + sqrtd) / a;
      if root < tmin || tmax < root {
        return None;
      }
    }

    let p = ray.at(root);
    let outward_normal = (p - self.center) / self.radius;

    Some(Hit::face_normal(
      p,
      root,
      ray,
      outward_normal,
      self.material,
    ))
  }
}

pub enum Object<'a> {
  Sphere(Sphere<'a>),
}

impl<'a> Hittable for Object<'a> {
  fn hit(&self, ray: &Ray, tmin: f32, tmax: f32) -> Option<Hit> {
    match self {
      Object::Sphere(sp) => sp.hit(ray, tmin, tmax),
    }
  }
}

impl<'a> Hittable for Vec<Object<'a>> {
  fn hit(&self, ray: &Ray, tmin: f32, tmax: f32) -> Option<Hit> {
    self
      .iter()
      .fold(
        (tmax, None),
        |(closest_so_far, closest_hit), obj| match obj.hit(ray, tmin, closest_so_far) {
          Some(hit) => (hit.t, Some(hit)),
          None => (closest_so_far, closest_hit),
        },
      )
      .1
  }
}
