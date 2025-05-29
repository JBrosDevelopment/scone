# Scone Programming Language *(in development)*

**Scone** is a simple yet powerful programming language currently in development.  
Its syntax takes inspiration from Rust, C, and Java, aiming to be **familiar**, **readable**, and **efficient**.

### Key Features
- Clean, familiar syntax (inspired by Rust, C, and Java)
- Operator overloading
- Type inference
- Traits (like `ToString`)
- Strong typing with explicit type declarations
- Call tagged `crumb` functions without scoping when creating an instance of a class or struct

## Example Code

```rust
pub class Color -> ToString {
    u8: R = 0;
    u8: G = 0;
    u8: B = 0;

    #! crumb
    #! description "Creates a color object with given RGB values"
    static Color: new(u8: r, u8: g, u8: b) {
        Color {
            R = r,
            G = g,
            B = b
        }
    }

    #! description "Creates a white color object"
    static Color: white() {
        Color {
            R = 255,
            G = 255,
            B = 255
        }
    }

    #! description "Operator overload for adding two colors" 
    static Color: op_add(Color: color1, Color: color2) {
        Color {
            R = color1.R + color2.R,
            G = color1.G + color2.G,
            B = color1.B + color2.B
        }
    }

    #! "Implementation of the ToString trait"
    string: to_string() {
        format("Color: R: {}, G: {}, B: {}", R, G, B)
    }
}

void: main() {
    // Create a new Color object with default values
    Color: color1 = Color {};

    // The crumb tag allows the compiler to infer the `new` function
    Color: color2 = new(255, 255, 255);

    // Accessing a static method that returns a white Color object
    Color: white = Color::white();

    // Calling an instance method (implements ToString)
    print(color2.to_string());

    // Type inference with `auto`
    auto: red = color2.R; 

    // Operator overloading
    Color: color3 = color1 + color2;
}

```

## Contributing

Contributions, feedback, and suggestions are welcome!
Stay tuned for updates as the language evolves.
