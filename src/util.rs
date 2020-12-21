use std::f64::consts::PI;

pub fn clamp(i: f64, min: f64, max: f64) -> f64 {
  i.max(min).min(max)
}

pub fn degrees_to_radians(degrees: f64) -> f64 {
  degrees * PI / 180.0
}
