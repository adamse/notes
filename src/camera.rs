use crate::ray::*;
use crate::vec3::*;

pub struct SimpleCamera {}

#[allow(non_upper_case_globals)]
impl SimpleCamera {
  const aspect_ratio: f32 = 16.0 / 9.0;
  const viewport_height: f32 = 2.0;
  const viewport_width: f32 = Self::aspect_ratio * Self::viewport_height;
  const focal_length: f32 = 1.0;

  pub fn ray(u: f32, v: f32) -> Ray {
    let origin = Vec3::zero();
    let horizontal = vec(Self::viewport_width, 0.0, 0.0);
    let vertical = vec(0.0, Self::viewport_height, 0.0);
    let lower_left_corner =
      origin - horizontal / 2.0 - vertical / 2.0 - vec(0.0, 0.0, Self::focal_length);

    ray(
      origin,
      lower_left_corner + u * horizontal + v * vertical - origin,
    )
  }
}
