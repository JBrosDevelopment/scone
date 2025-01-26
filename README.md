# Scone Programming Language (*in development*)

The scone programming language is a straightforward language that is designed to be syntactically like Rust, C, and Java. 

The language is still in development. This is what the syntax will look like:

```rust 

pub class Color <- ToString -> "Example for a color class" {
    u8: R = 0 -> "Red color code value 0 - 255";
    u8: G = 0 -> "Green color code value 0 - 255";
    u8: B = 0 -> "Blue color code value 0 - 255";

    #! alias
    Color::new(u8: r, u8: g, u8: b) -> "Creates a color object with given RGB values" {
        Color {
            R = r,
            G = g,
            B = b
        }
    }

    Color::white() -> "Creates a white color object" {
        Color {
            R = 255,
            G = 255,
            B = 255
        }
    }

    Color::op_add(Color: color1, Color: color2) -> "Operator overload for adding two colors" {
        Color {
            R = color1.R + color2.R,
            G = color1.G + color2.G,
            B = color1.B + color2.B
        }
    }

    string: to_string() -> "Implementation of the ToString interface" {
        format("Color: R: {}, G: {}, B: {}", R, G, B)
    }
}

void: main() {
    // creating a new object, no properties were provided, so the default values will be used
    Color: color1 = Color {};

    // because of alias tag, the compiler will infer the correct function
    Color: color2 = new(255, 255, 255);

    // accessing static members
    Color: white = Color::white();
    
    // accessing non-static members
    print(color2.to_string());

    // type inference
    auto: red = color2.R; 

    // operator overloading
    Color: color3 = color1 + color2;
}

```
