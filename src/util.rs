use std::f32::consts::PI;

pub fn clamp(i: f32, min: f32, max: f32) -> f32 {
  i.max(min).min(max)
}

pub fn degrees_to_radians(degrees: f32) -> f32 {
  degrees * PI / 180.0
}
