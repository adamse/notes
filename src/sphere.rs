use crate::hit::*;
use crate::ray::*;
use crate::vec3::*;

pub struct Sphere {
    pub center: Vec3,
    pub radius: f64,
}

impl Hittable for Sphere {
    fn hit(&self, ray: &Ray, tmin: f64, tmax: f64) -> Option<Hit> {
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
        Some(Hit {
            p,
            t: root,
            norm: (ray.at(root) - self.center) / self.radius,
        })
    }
}
