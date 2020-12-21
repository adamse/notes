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

use rand::{thread_rng, Rng};
use std::f32::INFINITY;
use std::fs::File;
use std::io::prelude::*;

enum Object {
  Sphere(Sphere),
}

impl Hittable for Object {
  fn hit(&self, ray: &Ray, tmin: f32, tmax: f32) -> Option<Hit> {
    match self {
      Object::Sphere(sp) => sp.hit(ray, tmin, tmax),
    }
  }
}

impl Hittable for Vec<Object> {
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

fn ray_colour(ray: &Ray, world: &Vec<Object>, depth: u32) -> Vec3 {
  if depth == 0 {
    return Vec3::zero();
  }

  if let Some(hit) = world.hit(ray, 0.0, INFINITY) {
    let target = hit.p + hit.norm + Vec3::random_unit_sphere();
    return 0.5 * ray_colour(&ray::ray(hit.p, target - hit.p), world, depth - 1);
  }

  let unit_direction = ray.dir.unit();
  let t = 0.5 * (unit_direction.y + 1.0);

  (1.0 - t) * Vec3::one() + t * colour(0.5, 0.7, 1.0)
}

fn main() -> std::io::Result<()> {
  // image
  let aspect_ratio = 16.0 / 9.0;
  let image_width = 400;
  let image_height = (image_width as f32 / aspect_ratio) as u32;

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
  let max_depth = 50;

  let mut file = File::create("img.ppm")?;

  file.write_all("P3\n".as_bytes())?;
  file.write_all(format!("{} {}\n", image_width, image_height).as_bytes())?;
  file.write_all("255\n".as_bytes())?;

  let samples_per_pixel = 100;
  let mut rnd = thread_rng();

  // render
  for j in (0..image_height).rev() {
    eprint!("\rscanlines remaining: {:5}", j);

    for i in 0..image_width {
      let colour = (0..samples_per_pixel).fold(Vec3::zero(), |colour, _sample| {
        let u = (i as f32 + rnd.gen_range(0.0..1.0)) / (image_width - 1) as f32;
        let v = (j as f32 + rnd.gen_range(0.0..1.0)) / (image_height - 1) as f32;

        let ray = camera::SimpleCamera::ray(u, v);

        colour + ray_colour(&ray, &world, max_depth)
      });

      let s = format!("{}\n", colour.colour_fmt(samples_per_pixel));
      let _ = file.write_all(s.as_bytes());
    }
  }
  eprintln!("\ndone");

  Ok(())
}
