use image::{Rgb, RgbImage};

fn main() {
    let goban_size = 695;
    let mut img = RgbImage::new(goban_size, goban_size);

    // Fill-in background
    for pixel in img.pixels_mut() {
        *pixel = Rgb([220, 179, 92]);
    }

    // Fill-in grid
    let line_color = Rgb([0, 0, 0]);
    let line_thickness = 2;
    let square_size = 34;
    let padding = 30;
    for i in 0..19 {
        let pos = padding + i * square_size + i * line_thickness;
        // a vertical line
        for t in 0..line_thickness {
            for y in padding..(padding + 18 * square_size + 19 * line_thickness) {
                img.put_pixel(pos + t, y, line_color);
            }
        }
        // a horizontal line
        for t in 0..line_thickness {
            for x in padding..(padding + 18 * square_size + 19 * line_thickness) {
                img.put_pixel(x, pos + t, line_color);
            }
        }
    }

    // Save as PNG
    img.save("output.png").expect("Failed to save image");
}
