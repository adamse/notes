use crate::ray::*;
use crate::vec3::*;

pub struct Hit {
    pub p: Vec3,
    pub norm: Vec3,
    pub t: f64,
}

pub trait Hittable {
    fn hit(&self, ray: &Ray, tmin: f64, tmax: f64) -> Option<Hit>;
}
