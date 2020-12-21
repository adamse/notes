mod vec3;
use vec3::*;
mod ray;
use ray::*;
mod hit;
use hit::*;
mod sphere;
use sphere::*;
mod camera;
mod util;

use std::f64::INFINITY;
use std::fs::File;
use std::io::prelude::*;

enum Object {
  Sphere(Sphere),
}

impl Hittable for Object {
  fn hit(&self, ray: &Ray, tmin: f64, tmax: f64) -> Option<Hit> {
    match self {
      Object::Sphere(sp) => sp.hit(ray, tmin, tmax),
    }
  }
}

impl Hittable for Vec<Object> {
  fn hit(&self, ray: &Ray, tmin: f64, tmax: f64) -> Option<Hit> {
    let mut closest_so_far = tmax;
    self.iter().fold(None, |closest_hit, obj| {
      if let Some(hit) = obj.hit(ray, tmin, closest_so_far) {
        closest_so_far = hit.t;
        Some(hit)
      } else {
        closest_hit
      }
    })
  }
}

fn ray_colour(ray: &Ray, world: &Vec<Object>) -> Vec3 {
  if let Some(hit) = world.hit(ray, 0.0, INFINITY) {
    return 0.5 * (hit.norm + Vec3::one());
  }
  let unit_direction = ray.dir.unit();
  let t = 0.5 * (unit_direction.y + 1.0);

  (1.0 - t) * colour(1.0, 1.0, 1.0) + t * colour(0.5, 0.7, 1.0)
}

fn main() -> std::io::Result<()> {
  // image
  let aspect_ratio = 16.0 / 9.0;
  let image_width = 400;
  let image_height = (image_width as f64 / aspect_ratio) as u32;

  let world = vec![
    Object::Sphere(Sphere {
      center: point(0.0, 0.0, -1.0),
      radius: 0.5,
    }),
    Object::Sphere(Sphere {
      center: point(0.0, -100.5, -1.0),
      radius: 100.0,
    }),
  ];

  let mut file = File::create("img.ppm")?;

  file.write_all("P3\n".as_bytes())?;
  file.write_all(format!("{} {}\n", image_width, image_height).as_bytes())?;
  file.write_all("255\n".as_bytes())?;

  // render
  for j in (0..image_height).rev() {

    eprint!("\rscanlines remaining: {:5}", j);

    for i in 0..image_width {

      let u = i as f64 / (image_width - 1) as f64;
      let v = j as f64 / (image_height - 1) as f64;

      let ray = camera::SimpleCamera::ray(u, v);

      let c = ray_colour(&ray, &world);

      let s = format!("{}\n", c.colour_fmt(1));
      let _ = file.write_all(s.as_bytes());
    }
  }
  eprintln!("\ndone");

  Ok(())
}
