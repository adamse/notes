mod vec3;
use vec3::*;
mod ray;
use ray::*;
mod hit;
use hit::*;
mod camera;
use camera::*;
mod object;
mod util;
use object::*;
mod material;
use material::*;
mod fastrand;
use fastrand::*;

use std::f32::INFINITY;
use std::fs::File;
use std::io::prelude::*;

fn ray_colour(ray: &Ray, world: &Vec<Object>, depth: u32) -> Vec3 {
  if depth == 0 {
    // ray has bounced enough
    return Vec3::zero();
  }

  if let Some(hit) = world.hit(ray, 0.001, INFINITY) {
    if let Some(scattered) = hit.material.scatter(ray, &hit) {
      return scattered.attenuation * ray_colour(&scattered.scattered, world, depth - 1);
    }
    // ray was absorbed
    return Vec3::zero();
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

  let material_ground = Material::Lambertian(vec(0.8, 0.8, 0.0));
  let material_center = Material::Lambertian(vec(0.1, 0.2, 0.5));
  let material_left = Material::Dielectric(1.5);
  let material_right = Material::Metal(vec(0.8, 0.6, 0.2), 0.0);
  // let material_center = Material::Lambertian(vec(0.7, 0.3, 0.3));
  // let material_left = Material::Metal(vec(0.8, 0.8, 0.8), 0.3);

  let world = vec![
    Object::Sphere(Sphere {
      center: point(0.0, -100.5, -1.0),
      radius: 100.0,
      material: material_ground,
    }),
    Object::Sphere(Sphere {
      center: point(0.0, 0.0, -1.0),
      radius: 0.5,
      material: material_center,
    }),
    Object::Sphere(Sphere {
      center: point(-1.0, 0.0, -1.0),
      radius: 0.5,
      material: material_left,
    }),
    Object::Sphere(Sphere {
      center: point(-1.0, 0.0, -1.0),
      radius: -0.4,
      material: material_left,
    }),
    Object::Sphere(Sphere {
      center: point(1.0, 0.0, -1.0),
      radius: 0.5,
      material: material_right,
    }),
  ];

  // let r = (PI / 4.0).cos();
  // let material_left = Material::Lambertian(colour(0.0, 0.0, 1.0));
  // let material_right = Material::Lambertian(colour(1.0, 0.0, 0.0));

  // let world = vec![
  //   Object::Sphere(Sphere {
  //     center: point(-r, 0.0, -1.0),
  //     radius: r,
  //     material: material_left,
  //   }),
  //   Object::Sphere(Sphere {
  //     center: point(r, 0.0, -1.0),
  //     radius: r,
  //     material: material_right,
  //   }),
  // ];

  let camera = FovCamera::new(
    point(-2.0, 2.0, 1.0),
    point(0.0, 0.0, -1.0),
    vec(0.0, 1.0, 0.0),
    20.0,
    aspect_ratio,
  );

  let max_depth = 50;

  let mut file = File::create("img.ppm")?;

  file.write_all("P3\n".as_bytes())?;
  file.write_all(format!("{} {}\n", image_width, image_height).as_bytes())?;
  file.write_all("255\n".as_bytes())?;

  let samples_per_pixel = 100;

  // render
  for j in (0..image_height).rev() {
    eprint!("\rscanlines remaining: {:5}", j);

    for i in 0..image_width {
      let colour = (0..samples_per_pixel).fold(Vec3::zero(), |colour, _sample| {
        let u = (i as f32 + rand_f32_01()) / (image_width - 1) as f32;
        let v = (j as f32 + rand_f32_01()) / (image_height - 1) as f32;

        let ray = camera.ray(u, v);

        colour + ray_colour(&ray, &world, max_depth)
      });

      let s = format!("{}\n", colour.colour_fmt(samples_per_pixel));
      let _ = file.write_all(s.as_bytes());
    }
  }
  eprintln!("\ndone");

  Ok(())
}
