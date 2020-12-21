use crate::hit::*;
use crate::ray::*;
use crate::vec3::*;

pub enum Material {
  Lambertian(f32),
}

pub struct Scatter {
  pub attenuation: f32,
  pub scattered: Ray,
}

impl Material {
  // None => the ray was absorbed
  pub fn scatter(&self, _: &Ray, hit: &Hit) -> Option<Scatter> {
    match self {
      Material::Lambertian(albedo) => {
        // could scatter with prob p and have attenuation albedo/p, to try
        let scatter_direction = hit.norm + Vec3::random_in_unit_vector();

        Some(Scatter {
          attenuation: *albedo,
          scattered: ray(
            hit.p,
            if scatter_direction.near_zero() {
              hit.norm
            } else {
              scatter_direction
            },
          ),
        })
      }
    }
  }
}
