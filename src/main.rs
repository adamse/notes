mod vec3;
use vec3::*;

const H: u32 = 256;
const W: u32 = 256;

fn print_ppm() {
    println!("P3");
    println!("{} {}", H, W);
    println!("255");

    for j in (0..H).rev() {
        eprint!("\rscanlines remaining: {:5}", j);
        for i in 0..W {
            let c = Vec3 {
                x: (i as f64) / ((W - 1) as f64),
                y: (j as f64) / ((H - 1) as f64),
                z: 0.25,
            };
            println!("{}", c.colour_fmt())
        }
    }
    eprintln!("\ndone");
}

fn main() {
    print_ppm();
}
