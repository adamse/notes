mod vec3;
use vec3::*;
mod ray;
use ray::*;
mod hit;
use hit::*;
mod camera;
use camera::*;
mod object;
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

fn random_scene() -> Vec<Object> {
  let mut world = Vec::new();

  world.push(Object::Sphere(Sphere {
    center: point(0.0, -1000.0, 0.0),
    radius: -1000.0,
    material: Material::Lambertian(colour(0.5, 0.5, 0.5)),
  }));

  for a in -11..11 {
    for b in -11..11 {
      let center = point(a as f32 + 0.9 * rand_f32_01(), 0.2, b as f32 + 0.9 * rand_f32_01());
      if (center - point(4.0, 0.2, 0.0)).length() > 0.9 {
        let r = rand_f32_01();
        if r < 0.8 {
          // diffuse
          let albedo = Vec3::random_in_unit_vector() * Vec3::random_in_unit_vector();
          world.push(Object::Sphere(Sphere {
            center,
            radius: 0.2,
            material: Material::Lambertian(albedo),
          }));
        } else if r < 0.95 {
          let albedo = Vec3::random(0.5, 1.0);
          let fuzz = rand_f32_interval(0.0, 0.5);
          world.push(Object::Sphere(Sphere {
            center,
            radius: 0.2,
            material: Material::Metal(albedo, fuzz),
          }));
        } else {
          world.push(Object::Sphere(Sphere {
            center,
            radius: 0.2,
            material: Material::Dielectric(1.5),
          }));
        }
      }
    }
  }

  world.push(Object::Sphere(Sphere {
    center: point(0.0, 1.0, 0.0),
    radius: 1.0,
    material: Material::Dielectric(1.5),
  }));
  world.push(Object::Sphere(Sphere {
    center: point(-4.0, 1.0, 0.0),
    radius: 1.0,
    material: Material::Lambertian(colour(0.4, 0.2, 0.1)),
  }));
  world.push(Object::Sphere(Sphere {
    center: point(4.0, 1.0, 0.0),
    radius: 1.0,
    material: Material::Metal(colour(0.7, 0.6, 0.5), 0.0),
  }));

  world
}

fn main() -> std::io::Result<()> {
  // let material_ground = Material::Lambertian(vec(0.8, 0.8, 0.0));
  // let material_center = Material::Lambertian(vec(0.1, 0.2, 0.5));
  // let material_left = Material::Dielectric(1.5);
  // let material_right = Material::Metal(vec(0.8, 0.6, 0.2), 0.0);
  // let material_center = Material::Lambertian(vec(0.7, 0.3, 0.3));
  // let material_left = Material::Metal(vec(0.8, 0.8, 0.8), 0.3);

  // let world = vec![
  //   Object::Sphere(Sphere {
  //     center: point(0.0, -100.5, -1.0),
  //     radius: 100.0,
  //     material: material_ground,
  //   }),
  //   Object::Sphere(Sphere {
  //     center: point(0.0, 0.0, -1.0),
  //     radius: 0.5,
  //     material: material_center,
  //   }),
  //   Object::Sphere(Sphere {
  //     center: point(-1.0, 0.0, -1.0),
  //     radius: 0.5,
  //     material: material_left,
  //   }),
  //   Object::Sphere(Sphere {
  //     center: point(-1.0, 0.0, -1.0),
  //     radius: -0.4,
  //     material: material_left,
  //   }),
  //   Object::Sphere(Sphere {
  //     center: point(1.0, 0.0, -1.0),
  //     radius: 0.5,
  //     material: material_right,
  //   }),
  // ];

  let world = random_scene();

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

  // image
  let aspect_ratio = 3.0 / 2.0; // 16.0 / 9.0;
  let image_width = 1200;
  let image_height = (image_width as f32 / aspect_ratio) as u32;
  let samples_per_pixel = 500;

  let lookfrom = point(13.0, 2.0, 3.0);
  let lookat = point(0.0, 0.0, 0.0);
  let dist_to_focus = 10.0;
  let aperture = 0.1;
  let camera = FovCamera::new(
    lookfrom,
    lookat,
    vec(0.0, 1.0, 0.0),
    20.0,
    aspect_ratio,
    aperture,
    dist_to_focus
  );

  let max_depth = 50;

  let mut file = File::create("img.ppm")?;

  file.write_all("P3\n".as_bytes())?;
  file.write_all(format!("{} {}\n", image_width, image_height).as_bytes())?;
  file.write_all("255\n".as_bytes())?;

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
