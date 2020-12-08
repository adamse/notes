mod vec3;
use vec3::*;
mod ray;
use ray::*;
mod hit;
use hit::*;
mod sphere;
use sphere::*;

fn ray_colour(ray: &Ray) -> Vec3 {
    let sphere = Sphere{
      center: point(0.0, 0.0, -1.0),
      radius: 0.5,
    };
    if let Some(hit) = sphere.hit(ray, -1000000.0, 1000000.0) { // hit_sphere(point(0.0, 0.0, -1.0), 0.5, ray) {
        return 0.5 * (hit.norm + Vec3::one());
    }
    let unit_direction = ray.dir.unit();
    let t = 0.5 * (unit_direction.y + 1.0);

    (1.0 - t) * colour(1.0, 1.0, 1.0) + t * colour(0.5, 0.7, 1.0)
}

fn main() {
    // image
    let aspect_ratio = 16.0 / 9.0;
    let image_width = 400;
    let image_height = (image_width as f64 / aspect_ratio) as u32;

    // camera
    let viewport_height = 2.0;
    let viewport_width = aspect_ratio * viewport_height;
    let focal_length = 1.0;

    let origin = Vec3::zero();
    let horizontal = vec(viewport_width, 0.0, 0.0);
    let vertical = vec(0.0, viewport_height, 0.0);
    let lower_left_corner =
        origin - horizontal / 2.0 - vertical / 2.0 - vec(0.0, 0.0, focal_length);

    // render
    foreach_pixel(image_width, image_height, |i, j| {
        let u = i as f64 / (image_width - 1) as f64;
        let v = j as f64 / (image_height - 1) as f64;
        let ray = ray(
            origin,
            lower_left_corner + u * horizontal + v * vertical - origin,
        );
        ray_colour(&ray)
    });
}

fn foreach_pixel<F>(width: u32, height: u32, fun: F)
where
    F: Fn(u32, u32) -> Vec3,
{
    println!("P3");
    println!("{} {}", width, height);
    println!("255");
    for j in (0..height).rev() {
        eprint!("\rscanlines remaining: {:5}", j);
        for i in 0..width {
            println!("{}", fun(i, j).colour_fmt());
        }
    }
    eprintln!("\ndone");
}

// fn print_ppm() {
//     const H: u32 = 256;
//     const W: u32 = 256;
//     foreach_pixel(W, H, |i, j| {
//         colour(
//             (i as f64) / ((W - 1) as f64),
//             (j as f64) / ((H - 1) as f64),
//             0.25,
//         )
//     });
// }
