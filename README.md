# Scone Programming Language *(in development)*

**Scone** is a simple yet powerful programming language currently in development.  
Its syntax takes inspiration from Rust, C, and Java, aiming to be **familiar**, **readable**, and **efficient**.

### Key Features
- Clean, familiar syntax (inspired by Rust, C, and Java)
- Operator overloading
- Type inference
- Interfaces and traits (like `ToString`)
- Aliased constructors for intuitive object creation
- Strong typing with explicit type declarations

## Example Code

```rust
pub class Color <- ToString -> "Example of a color class" {
    u8: R = 0 -> "Red color value (0 - 255)";
    u8: G = 0 -> "Green color value (0 - 255)";
    u8: B = 0 -> "Blue color value (0 - 255)";

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
    // Create a new Color object with default values (R: 0, G: 0, B: 0)
    Color: color1 = Color {};

    // Thanks to the alias tag, the compiler infers the constructor function here
    Color: color2 = new(255, 255, 255);

    // Accessing a static method that returns a white Color object
    Color: white = Color::white();

    // Calling an instance method (implements ToString)
    print(color2.to_string());

    // Type inference with 'auto'
    auto: red = color2.R; 

    // Operator overloading in action (adds the color values together)
    Color: color3 = color1 + color2;
}

```

## Contributing

Contributions, feedback, and suggestions are welcome!
Stay tuned for updates as the language evolves.
