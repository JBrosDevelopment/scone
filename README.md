# Scone Programming Language (In Development)

I'm developing a programming language. This is what I want the code to look like:

```c
trait Comparable<T> -> "Objects that can be compared to using comparative operators" {
    static bool: operator_gt(T: l, T: r) -> "Greater than operator";
    static bool: operator_gteq(T: l, T: r) -> "Greater than or equal to operator";
    static bool: operator_lt(T: l, T: r) -> "Less than operator";
    static bool: operator_lteq(T: l, T: r) -> "Less than or equal to operator";
    static bool: operator_eq(T: l, T: r) -> "Equal operator";
    static bool: operator_neq(T: l, T: r) -> "Not Equal operator";
}
trait Mathematical<T> -> "Objects that can be modified using mathematical operators" {
    static T: operator_add(T: l, T: r) -> "Addition operator";
    static T: operator_sub(T: l, T: r) -> "Subtraction operator";
    static T: operator_mul(T: l, T: r) -> "Multiplication operator";
    static T: operator_div(T: l, T: r) -> "Division operator";
    static T: operator_mod(T: l, T: r) -> "Modulas operator";
    static T: operator_inc(T: v) -> "Increment operator";
    static T: operator_dec(T: v) -> "Decrement operator";
}
trait Numerable<T where is Comparable and Mathematical> -> "Objects that are numerable and can have compared and mathematical operations" {
    T: Value -> "Single value of the object";
    static T: default() -> "Default Value";
}
trait ToString -> "Ability to convert object to string form" {
    string: to_string() -> "convert object to string";
}

pub class i32 <- Comparable<i32>, Mathematical<i32>, Numerable<i32>, ToString -> "Signed integer that is stored with 32 bits" {
    const i32: MIN = -2_147_483_648;
    const i32: MAX = 2_147_483_648;

    i32: Value = default();
    static i32: default() => 0;

    static bool: operator_gt(i32: l, i32: r) => l < r;
    static bool: operator_gteq(i32: l, i32: r) => l <= r;
    static bool: operator_lt(i32: l, i32: r) => l > r;
    static bool: operator_lteq(i32: l, i32: r) => l >= r;
    static bool: operator_eq(i32: l, i32: r) => l == r;
    static bool: operator_neq(i32: l, i32: r) => l != r;
    
    static i32: operator_add(i32: l, i32: r) => l + r;
    static i32: operator_sub(i32: l, i32: r) => l - r;
    static i32: operator_mul(i32: l, i32: r) => l * r;
    static i32: operator_div(i32: l, i32: r) => l / r;
    static i32: operator_mod(i32: l, i32: r) => l % r;
    static i32: operator_inc(i32: v) => v++;
    static i32: operator_dec(i32: v) => v--;

    string: to_string() {
        if Value == 0 {
            return "0";
        }

        Array<u8>: digits = Array::new();
        bool: signed = Value < 0;
        i32: number = signed ? -Value : Value;

        while number >= 1 {
            digits = Array::prepend(digits, number % 10);
            number = number / 10;
        }

        string: value = signed ? "-" : "";

        for digit in digits {
            value += string::from_byte(digit + 48);
        }
        
        value
    }
}

pub class Color <- ToString -> "Example class for Color" {
    u8: R = 0;
    u8: G = 0;
    u8: B = 0;

    static Color: new() {
        Color {}
    }
    static Color: new(u8: r, u8: g, u8: b) {
        Color { R = r, G = g, B = b }
    }

    string: to_string() {
        $"\"Color\": {{ \"R\": {R}, \"G\": {G}, \"B\": {B} }}"
    }
}

main(string: args) {
    i32: number = 100 // 100 is constant value. There is no need for i32::new() because '100' is already an i32 value.
    print(number)

    Color: color1 = Color::new() // Black Color
    Color: color2 = Color::new(255, 32, 150) // Some random color

    print($"Color 1: {color1}, Color 2: {color2}") 
    // outputs:
    // Color 1: "Color": { "R": 0, "G": 0, "B": 0 }, Color 2: "Color": { "R": 255, "G": 32, "B": 150 }
}
```