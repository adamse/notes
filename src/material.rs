use crate::hit::*;
use crate::ray::*;
use crate::vec3::*;

use rand::{thread_rng, Rng};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Material {
  /// `(colour)`
  Lambertian(Vec3),
  /// `(colour, fuzziness)`
  Metal(Vec3, f32),
  /// `(refraction_index)`
  Dielectric(f32),
}

pub struct Scatter {
  pub attenuation: Vec3,
  pub scattered: Ray,
}

impl Material {
  /// `None` => the ray was absorbed
  pub fn scatter(self, ray_in: &Ray, hit: &Hit) -> Option<Scatter> {
    match self {
      Material::Lambertian(albedo) => {
        // could scatter with prob p and have attenuation albedo/p, to try
        let scatter_direction = hit.norm + Vec3::random_in_unit_vector();

        Some(Scatter {
          attenuation: albedo,
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

      Material::Metal(albedo, fuzz) => {
        let reflected = reflect(ray_in.dir.unit(), hit.norm);
        let scattered = ray(
          hit.p,
          reflected + fuzz.min(1.0) * Vec3::random_in_unit_sphere(),
        );

        if dot(scattered.dir, hit.norm) > 0.0 {
          Some(Scatter {
            attenuation: albedo,
            scattered,
          })
        } else {
          None
        }
      }

      Material::Dielectric(refraction_index) => {
        let refraction_ratio = if hit.front_face {
          1.0 / refraction_index
        } else {
          refraction_index
        };

        let unit_direction = ray_in.dir.unit();

        let cos_theta = dot(-unit_direction, hit.norm).min(1.0);
        let sin_theta = (1.0 - cos_theta * cos_theta).sqrt();

        let cannot_refract = refraction_ratio * sin_theta > 1.0;
        let reflected = reflectance(cos_theta, refraction_ratio) > thread_rng().gen::<f32>();

        let direction = if cannot_refract || reflected {
          reflect(unit_direction, hit.norm)
        } else {
          refract(unit_direction, hit.norm, refraction_ratio)
        };

        let attenuation = Vec3::one();
        let scattered = ray(hit.p, direction);

        Some(Scatter {
          attenuation,
          scattered,
        })
      }
    }
  }
}

fn reflectance(cosine: f32, refraction_index: f32) -> f32 {
  let r0 = ((1.0 - refraction_index) / (1.0 + refraction_index)).powi(2);
  r0 + (1.0 - r0) * (1.0 - cosine).powi(5)
}
