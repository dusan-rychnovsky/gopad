use image::{Rgb, RgbImage};
use imageproc::drawing::{draw_text_mut, draw_filled_circle_mut};
use rusttype::{Font, Scale};

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

    // Draw star points
    let star_color = Rgb([0, 0, 0]);
    let star_radius = 4;
    let star_points = [
        (3, 3), (3, 9), (3, 15),
        (9, 3), (9, 9), (9, 15),
        (15, 3), (15, 9), (15, 15)
    ];
    for &(i, j) in &star_points {
        let x = padding + i * square_size + ((i as f32 + 0.5) * line_thickness as f32) as u32;
        let y = padding + j * square_size + ((j as f32 + 0.5) * line_thickness as f32) as u32;
        draw_filled_circle_mut(&mut img, (x as i32, y as i32), star_radius, star_color);
    }

    // Load font
    let font_data = std::fs::read("resources/dejavu-sans.book.ttf").expect("Font file not found");
    let font = Font::try_from_vec(font_data).expect("Error constructing Font");
    let font_scale = Scale { x: 18.0, y: 18.0 };
    let text_color = Rgb([0, 0, 0]);

    let letters: Vec<char> = (b'A'..=b'S').map(|c| c as char).collect();

    // Annotate top
    for (i, &ch) in letters.iter().enumerate() {
        let i = i as u32;
        let font_size = 10;
        let x = padding + i * square_size + (i + 1) * line_thickness - font_size / 2;
        let top_margin = 7;
        draw_text_mut(&mut img, text_color, x as i32, top_margin as i32, font_scale, &font, &ch.to_string());
    }

    // Annotate left
    for (i, &ch) in letters.iter().enumerate() {
        let i = i as u32;
        let font_size = 18;
        let left_margin = 11;
        let y = padding + i * square_size + (i + 1) * line_thickness - font_size / 2;
        draw_text_mut(&mut img, text_color, left_margin as i32, y as i32, font_scale, &font, &ch.to_string());
    }

    // Save as PNG
    img.save("output.png").expect("Failed to save image");
}
