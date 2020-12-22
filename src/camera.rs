use crate::ray::*;
use crate::util::*;
use crate::vec3::*;

// pub struct SimpleCamera {}

// #[allow(non_upper_case_globals)]
// impl SimpleCamera {
//   const aspect_ratio: f32 = 16.0 / 9.0;
//   const viewport_height: f32 = 2.0;
//   const viewport_width: f32 = Self::aspect_ratio * Self::viewport_height;
//   const focal_length: f32 = 1.0;

//   pub fn ray(u: f32, v: f32) -> Ray {
//     let origin = Vec3::zero();
//     let horizontal = vec(Self::viewport_width, 0.0, 0.0);
//     let vertical = vec(0.0, Self::viewport_height, 0.0);
//     let lower_left_corner =
//       origin - horizontal / 2.0 - vertical / 2.0 - vec(0.0, 0.0, Self::focal_length);

//     ray(
//       origin,
//       lower_left_corner + u * horizontal + v * vertical - origin,
//     )
//   }
// }

pub struct FovCamera {
  /// point
  pub origin: Vec3,
  /// point
  pub lower_left_corner: Vec3,
  /// vec
  pub horizontal: Vec3,
  /// vec
  pub vertical: Vec3,

	pub w: Vec3,
	pub u: Vec3,
	pub v: Vec3,
  pub lens_radius: f32,
}

impl FovCamera {
  /// - `vfov`: vertical field-of-view in degrees
  pub fn new(
    lookfrom: Vec3,
    lookat: Vec3,
    vup: Vec3,
    vfov: f32,
    aspect_ratio: f32,
    aperture: f32,
    focus_dist: f32,
  ) -> Self {
    let theta = degrees_to_radians(vfov);
    let h = (theta / 2.0).tan();
    let viewport_height = 2.0 * h;
    let viewport_width = aspect_ratio * viewport_height;

    let w = (lookfrom - lookat).unit();
    let u = cross(vup, w).unit();
    let v = cross(w, u);

    let origin = lookfrom;
    let horizontal = focus_dist * viewport_width * u;
    let vertical = focus_dist * viewport_height * v;
    let lower_left_corner = origin - horizontal / 2.0 - vertical / 2.0 - focus_dist * w;
		let lens_radius = aperture / 2.0;

    FovCamera {
      origin,
      lower_left_corner,
      horizontal,
      vertical,
			w, u, v,
			lens_radius,
    }
  }

  pub fn ray(&self, u: f32, v: f32) -> Ray {
		let rd = self.lens_radius * Vec3::random_in_unit_disk();
		let offset = self.u * rd.x + self.v * rd.y;
    ray(
      self.origin + offset,
      self.lower_left_corner + u * self.horizontal + v * self.vertical - self.origin - offset,
    )
  }
}
