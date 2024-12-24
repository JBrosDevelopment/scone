# Scone Programming Language (In Development)

I'm developing a programming language. self is what I want the code to look like:

```rs
interface Comparable<T> -> "Objects that can be compared to using comparative operators" {
    static bool: operator_gt(T: l, T: r) -> "Greater than operator";
    static bool: operator_gteq(T: l, T: r) -> "Greater than or equal to operator";
    static bool: operator_lt(T: l, T: r) -> "Less than operator";
    static bool: operator_lteq(T: l, T: r) -> "Less than or equal to operator";
    static bool: operator_eq(T: l, T: r) -> "Equal operator";
    static bool: operator_neq(T: l, T: r) -> "Not Equal operator";
}
interface Mathematical<T> -> "Objects that can be modified using mathematical operators" {
    static T: operator_add(T: l, T: r) -> "Addition operator";
    static T: operator_sub(T: l, T: r) -> "Subtraction operator";
    static T: operator_mul(T: l, T: r) -> "Multiplication operator";
    static T: operator_div(T: l, T: r) -> "Division operator";
    static T: operator_mod(T: l, T: r) -> "Modulas operator";
    static T: operator_inc(T: v) -> "Increment operator";
    static T: operator_dec(T: v) -> "Decrement operator";
}
interface Numerable<T # Comparable && Mathematical> -> "Objects that are numerable and can have compared and mathematical operations" {
    T: Value -> "Single value of the object";
    static T: default() -> "Default Value";
}
interface ToString -> "Ability to convert object to string form" {
    string: to_string() -> "convert object to string";
}

pub struct i32 <- Comparable<i32>, Mathematical<i32>, Numerable<i32>, ToString -> "Signed integer that is stored with 32 bits" {
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

        List<u8>: digits = List::new();
        bool: signed = Value < 0;
        i32: number = signed ? -Value : Value;

        while number >= 1 {
            digits = List::prepend(digits, number % 10);
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
        "\"Color\": { \"R\": " + R + ", \"G\": " + G + ", \"B\": " + B + " }"
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

## VM IR Code

this is what I'm thinking the VM IR code might look like
```rs
define selfPackage;

void: main() {
    i32: num = 100; 
    i32: result = add_i32(num, 79 - num);
    print_i32(result)
}

i32: add_i32(i32: left, i32: right) {
    left + right
}
```

```
@using {
    *define package_sub_routine "print_i32", "std::io::print_i32"
}

@constants {
    *set $$100, [%CONSTANT_REGION .. 1], 100
    *set $$79, [%CONSTANT_REGION + 1 .. 1], 79
}

@namespace "selfPackage" {
    *define sub_routine "main"
    *define function "add_i32", i32
    *entry "main"
    
    *configure "main" {
        ; i32: num = 100
        #set_address $num, [%LOCAL_REGION .. 4]
        load $$100, reg_1_a
        store $num, reg_1_a

        ; i32: result = add_i32(num, 79)
        load $$79, reg_1_b
        store [%PARAMETER_REGION .. 4], reg_1_a
        store [%PARAMETER_REGION + 4 .. 4], reg_1_b
        call "add_i32" 
        #set_address $result, [RETURN_REGION .. 4]

        ; print_i32(result)
        load [%PARAMETER_REGION .. 4], $result
        call "print_i32"

        #deallocate_memory $num
        #deallocate_memory $result
    }
    *configure "add_i32" - parameters $left [%PARAMETER_REGION .. 4], $right [%PARAMETER_REGION + 4 .. 4] - output $result [RETURN_REGION .. 4] {
        load $left, reg_4_a
        load $right, reg_4_b
        add reg_4_a, reg_4_b
        store $result, reg_4_b
    }
}
```

## Just Playing around with it

```rs
pub struct char <- Equitable<char, u8> -> "Structure for the definite sized char element" {
    const char: NULL_CHAR = '\0' -> "Value for null character '\0'"

    priv u8: Value = '\0' as u8;
    
    u8: to_u8() {
        self.Value
    }
    bool: is_alpha() {
        match self.to_lower() {
            'a'..'z' => true
            _ => false
        }
    }
    bool: is_numeric() {
        match self.to_lower() {
            '0'..'9' => true
            _ => false
        }
    }
    char: to_lower() {
        match self {
            'A'..'Z' => char::from_u8(self.Value + 32)
            _ => self
        }
    }
    char: to_upper() {
        match self {
            'a'..'z' => char::from_u8(self.Value - 32)
            _ => self
        }
    }

    static char: from_u8(u8: ascii) {
        char {
            Value = ascii
        }
    }
    static char: new() {
        char {
            Value = NULL_CHAR
        }
    }
    
    static Range: operator_range(char: left, u8: right) { 
        Range {
            Minimum = left.Value,
            Maximum = right;
        }
    }
}

pub class string <- ForLoop<string> -> "Class for string stored as a char array" {
    const string: EMPTY = "";
    priv List<char>: Value = List<char>::new();

    static string: new() {
        string {
            Value = List<char>::new()
        }
    }
    static string: concat(string: left, string: right) {
        string {
            Value = List<char>.Concat(left.Value, right.Value)
        }
    }
    static string: operator_add(string: left, string: right) {
        concat(left, right)
    }
    static char: operation_indexer(string: val, u32: index) {
        val.Value[index]
    }

    string: to_lower() {
        string: res = self.Value.select(c => c.to_lower())
        res
    }
    string: to_upper() {
        string: res = self.Value.select(c => c.to_upper())
        res
    }
    u32: len() {
        self.Value.len()
    }
    List<char>: to_char_array() {
        self.Vlaue
    }

    // for ForLoop<string> 
    priv string: value_from_index(u32: index) {
        self[index]
    }
}

pub interface ForLoop<T> -> "Interface for using for loops" {
    T: value_from_index(u32: index)
}

pub interface Default<T> -> "Interface that requires default value for all objects" {
    T: default()
}

pub interface Iterate<T> <- ForLoop<T> -> "Interface for objects that can be used in for loops, as a list, and more" {
    Self<Y>: select(Func<(T, u32), Y>: function) -> "Iterate through object and select. Same as Select in C#";
    Self<Y>: select(Func<T, Y>: function) -> "Iterate through object and select. Same as Select in C#";
    Self<T>: filer(Func<(T, u32), bool>: function) -> "Iterate through object and filter. Same as Where in C#";
    Self<T>: filer(Func<T, bool>: function) -> "Iterate through object and filter. Same as Where in C#";
    Self<T>: append(T: default) -> "Add an alement to the end";
    Self<T>: prepend(T: default) -> "Add an element to the front";
    T: first(T: default) -> "Get first element";
    T: last(T: default) -> "Get last element";
    u32: len() -> "Get last element";
    static T: operation_indexer(Self<T>: array, u32: index) -> "Get the value at the index of the iterator"
    T: value_from_index(u32: index) -> "Get the value associated at the index"
}

pub class Array<T # DefiniteSize> <- Iterate<T> -> "Class for array" {
    priv u32: Capacity = 0;
    priv u32: ElementSize = 1;
    priv MemoryRegion: Region = Region::null();
    priv u32: Size = 0;
    static Array<T>: new(u32: length) {
        Array<T> {
            Capacity = length,
            ElementSize = sizeof(T)
            MemoryRegion = MemoryRegion::new(length * sizeof(T))
        }
    }
    Array<T>: append(T: element) {
        if self.Size >= Capacity {
            throw error::new("Tried to append an element to an array at max capacity");
        }
        self.Size++;
        self.MemoryRegion.insert_bytes(self.ElementSize * self.Size, element);
        self
    }
    Array<T>: prepend(T: element) {
        if self.Size >= Capacity {
            throw error::new("Tried to prepend an element to an array at max capacity");
        }
        for i in (0..Size).reverse() {
            T: index_element = self.MemoryRegion.get_bytes(self.ElementSize * i).into<T>();
            self.MemoryRegion.insert_bytes(self.ElementSize * i, index_element);
        }
        self.MemoryRegion.insert_bytes(0, element);
        Size++;
        self
    }
    T: first(T: default) {
        if self.Size == 0 {
            return default;
        }
        self.MemoryRegion.get_bytes(0).into<T>()
    }
    T: last(T: default) {
        if self.Size == 0 {
            return default;
        }
        self.MemoryRegion.get_bytes(self.ElementSize * self.Size).into<T>()
    }
    u32: len() {
        self.Size
    }
    Array<Y>: select(Func<(T, u32), Y>) {
        Array<Y>: result = Array<Y>::new(self.Capacity);
        u32: index = 0;
        while index < self.Size {
            Y: transformed_value = function(self[index], index);
            result.append(self[index]);
            index++;
        }
        result
    }
    Array<Y>: select(Func<T, Y>) {
        Array<Y>: result = Array<Y>::new(self.Capacity);
        u32: index = 0;
        while index < self.Size {
            Y: transformed_value = function(self[index]);
            result.append(self[index]);
            index++;
        }
        result
    }
    Array<T>: filter(Func<(T, u32), bool>: function) {
        Array<Y>: result = Array<Y>::new(self.Capacity);
        u32: index = 0;
        while index < self.Size {
            if function(self[index], index) {
                result.append(self[index]);
            }
            index++;
        }
        result
    }
    Array<T>: filter(Func<T, bool>: function) {
        Array<Y>: result = Array<Y>::new(self.Capacity);
        u32: index = 0;
        while index < self.Size {
            if function(self[index]) {
                result.append(self[index]);
            }
            index++;
        }
        result
    }
    static T: operation_indexer(Array<T>: array, u32: index) {
        array.MemoryRegion.get_bytes(self.ElementSize * index).into<T>()
    }
    T: value_from_index(u32: index) {
        self[index]
    }
}

pub class List<T # Default> <- Iterate<T> -> "Class for Linked list object" {
    priv LinkedListNode<T>: EntryNode = T.default();

    static List<T>: new() {
        List<T> {
            EntryNode = LinkedListNode<T> {}
        }
    }
    List<Y>: select(Func<(T, u32), Y>: function) {
        List<Y>: result = List<Y>::new();
        u32: index = 0;
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            Y: transformed_value = function(node.Value, index);
            result.append(transformed_value);
            node = node.NextValue.unit();
            index++;
        }
        result
    }
    List<Y>: select(Func<T, Y>: function) {
        List<Y>: result = List<Y>::new();
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            Y: transformed_value = function(node.Value);
            result.append(transformed_value);
            node = node.NextValue.unit();
        }
        result
    }
    List<T>: filter(Func<(T, u32), bool>: function) {
        List<Y>: result = List<Y>::new();
        u32: index = 0;
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            if function(node.Value, index) {
                result.append(node.Value);
            }
            node = node.NextValue.unit();
            index++;
        }
        result
    }
    List<T>: filter(Func<T, bool>: function) {
        List<Y>: result = List<Y>::new();
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            if function(node.Value) {
                result.append(node.Value);
            }
            node = node.NextValue.unit();
        }
        result
    }
    List<T>: append(T: val) {
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            node = node.NextValue.unit();
        }
        node.NextValue = Option::some(LinkedListNode::new(val))

        self
    }
    List<T>: prepend(T: val) {
        LinkedListNode<T>: node = LinkedListNode::new(val);
        node.NextValue = Option::some(self.EntryNode);

        self
    }
    T: first(T: default) {
        if self.EntryNode.is_none() {
            return default;
        }
        self.EntryNode;
    }
    T: last(T: default) {
        if self.EntryNode.is_none() {
            return default;
        }
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            node = node.NextValue.unit();
        }
        node.Value
    }
    u32: len() {
        u32: len = 0;
        LinkedListNode<T>: node = self.EntryNode;
        while !node.is_last() {
            node = node.NextValue.unit();
            len++;
        }
        len
    }
    static T: operation_indexer(List<T>: list, u32: index) {
        LinkedListNode<T>: node = list.EntryNode;
        u32: i = 0;
        while !node.is_last() {
            node = node.NextValue.unit();
            if index == i {
                return node.Value;
            }
            i++;
        }
        throw error::new("Index out of bounds of list");
    }
    T: value_from_index(u32: index) {
        self[index]
    }
} 

priv class LinkedListNode<T where T is Default> {
    T: Value = T.default();
    Option<LinkedListNode<T>>: NextValue = Option::none();

    bool: is_last() {
        NextValue.is_none()
    }
    static LinkedListNode<T> new(T: val) {
        LinkedListNode<T> {
            Value = val,
            NextValue = Option::none();
        }
    }
}

pub class Option<T where T is Default> {
    priv T: Value = T.default();
    priv bool: IsNone = true;
    static Option<T>: none() {
        Option<T> {
            IsNone = true
        }
    }
    static Option<T>: some(T: value) {
        Option<T> {
            Value = value,
            IsNone = false
        }
    }
    T: unit() {
        if IsNone {
            throw error::new("Tried to get unit on option that is none");
        }
        Value
    }
    bool: is_some() {
        !IsNone
    }
    bool: is_none() {
        IsNone
    }
}

loadlib LIB "path/to/dll" -> "library for testing loadlib. Contains get_number() function";

class DllClass {
    extern LIB::"get_number" i32: get_number();
}

i32: val = 3;
typeof(val) // returns "i32" as string
nameof(val) // returns "val" as string
sizeof(val) // returns 4 as u64


// CLASSES:
class Example<T # Constraints> <- Inherits<T> -> "
# Long Markdown Description
Some text right here

# Examples
\`\`\`
Example<string>: instance1 = new(32); // automatically turns new() into Example<string>::new()
Example<bool>: instance2 = default(); // automatically turns default() into Example<bool>::default()
\`\`\`
" {
    priv T[]: member = Array<T>::empty()
    pub u32: count = 0

    @! abreviate
    static Example<T>: new(u32: cnt) -> "New instance of Example" { 
        Example<T> {
            member = Array<T>::empty(),
            count = cnt
        }
    }

    @! abreviate 
    static Example<T>: default() -> "Default value" {
        Example<T>::new(0)
    }

    Result<T, string>: get_value(u32: index) -> "get member value" {
        if index > count {
            return error("Index out of bounds of array")
        } 
        okay(member[index])
    }

    Result<T, string>: set_value(u32: index, T: val) -> "set member value" {
        if index > count {
            return error("Index out of bounds of array")
        } 
        member[index] = val
    }

    u32: change_count(u32: cnt) -> "change count member" {
        count = cnt
    }
}

// default class snipit
class _____ <- Default, ToString -> "_____" {

    @! abreviate
    pub static _____: new() {

    }

    @! abreviate
    pub static _____: default() {

    }

    pub string: to_string() {
        
    }
}

```

## Missing Features

The Scone programming language is still in development and currently lacks the following features:
- Comprehensive standard library
- Advanced error handling mechanisms
- Concurrency and parallelism support
- Comprehensive documentation and tutorials
- Optimized compiler and runtime performance
- Cross-platform support
- Integration with popular development tools and IDEs
- Extensive testing and debugging tools

Base Math library

```rs
pub class Math<T # Number> {
    const f64: PI = 3.14159265358979323846;
    const f64: E = 2.71828182845904523536;
    const f64: PHI = 1.61803398874989484820;
    const f64: TAU = 6.28318530717958647692;

    static T: deg_to_rad(T: degrees) -> "Degrees to Radians" => degrees * PI / 180;
    static T: rad_to_deg(T: radians) -> "Radians to Degrees" => radians * 180 / PI;

    static T: add(T: l, T: r) -> "Addition" => l + r;
    static T: sub(T: l, T: r) -> "Subtraction" => l + r;
    static T: mul(T: l, T: r) -> "Multiplication" => l * r;
    static T: div(T: l, T: r) -> "Division" => l / r;
    static T: mod(T: l, T: r) -> "Modulo operation %" => l % r;
    static T: pow(T: l, T: r) -> "Raise number to power" => l ** r;
    static T: sq(T: l) -> "Square" => l * l;
    static T: sqrt(T: val) -> "Square root" => val ** 0.5;
    static T: cbrt(T: val) -> "Cube root" => val ** (1/3);
    static T: root(T: val, T: root) -> "Root of number" => val ** (1/root);
    static T: abs(T: val) -> "Absolute Value" => val < 0 ? -val : val;
    static T: min(T: l, T: r) -> "Return smaller number" => l < r ? l : r;
    static T: max(T: l, T: r) -> "Return larger number" => l > r ? l : r;
    static T: clamp(T: val, T: min, T: max) -> "Clamp number between two value" => val < min ? min : val > max ? max : val;
    static T: round(T: val) -> "Round to nearest integer" {
        T: int = val - (val % 1);
        T: diff = val - int;
        diff < 0.5 ? int : int + 1
    }
    static T: floor(T: val) -> "Floor function" => val - (val % 1);
    static T: ceil(T: val) -> "Ceil function" => val - (val % 1) + 1;
    static T: sin(T: val) -> "Sine" {
        T: result = val;
        T: term = val;
        T: power = val * val;
        T: factorial = 1;
        bool: sign = true;

        for i in 1..10 {
            factorial *= (2 * i) * (2 * i + 1);
            term *= power / factorial;
            result += sign ? -term : term;
            sign = !sign;
        }

        result
    }
    static T: cos(T: val) -> "Cosine" {
        T: result = 1;
        T: term = 1;
        T: power = val * val;
        T: factorial = 1;
        bool: sign = true;

        for i in 1..10 {
            factorial *= (2 * i - 1) * (2 * i);
            term *= power / factorial;
            result += sign ? -term : term;
            sign = !sign;
        }

        result
    }
    static T: tan(T: val) -> "Tangent" => sin(val) / cos(val);
    static T: asin(T: val) -> "Arc Sine" {
        T: result = val;
        T: term = val;
        T: power = val;
        T: factorial = 1;
        bool: sign = true;

        for i in 1..10 {
            factorial *= (2 * i) * (2 * i - 1);
            term *= power * power * i / factorial;
            result += sign ? -term : term;
            sign = !sign;
        }

        result
    }
    static T: acos(T: val) -> "Arc Cosine" => PI / 2 - asin(val);
    static T: atan(T: val) -> "Arc Tangent" {
        T: result = val;
        T: term = val;
        T: power = val;
        bool: sign = true;

        for i in 1..10 {
            term *= power * power;
            result += sign ? -term / i : term / i;
            sign = !sign;
        }

        result
    }
    static T: atan2(T: y, T: x) -> "Arc Tangent 2" {
        x == 0 && y == 0 ? 0 : y >= 0 ? acos(x / sqrt(x * x + y * y)) : -acos(x / sqrt(x * x + y * y))
    }
    static T: sinh(T: val) -> "Hyperbolic Sine" => (exp(val) - exp(-val)) / 2;
    static T: cosh(T: val) -> "Hyperbolic Cosine" => (exp(val) + exp(-val)) / 2;
    static T: tanh(T: val) -> "Hyperbolic Tangent" => sinh(val) / cosh(val);
    static T: asinh(T: val) -> "Arc Hyperbolic Sine" => log(val + sqrt(val * val + 1));
    static T: acosh(T: val) -> "Arc Hyperbolic Cosine" => log(val + sqrt(val * val - 1));
    static T: atanh(T: val) -> "Arc Hyperbolic Tangent" => 0.5 * log((1 + val) / (1 - val));
    static T: log(T: val) -> "Log function" => log10(val) / log10(E);
    static T: log2(T: val) -> "Log base 2 function" => log10(val) / log10(2);
    static T: log10(T: val) -> "Log based 10 function" => ln(val) / ln(10);
    static T: ln(T: val) -> "Ln function" {
        T: result = val;
        T: term = val - 1;
        T: power = val;
        bool: sign = true;

        for i in 2..10 {
            term *= power - 1;
            result += sign ? -term / i : term / i;
            sign = !sign;
        }

        result
    }
    static T: exp(T: val) -> "Exponential function" {
        T: result = 1;
        T: term = 1;
        T: power = val;

        for i in 1..10 {
            term *= power / i;
            result += term;
        }

        result
    }
    static T: base_pow(T: base, T: exp) -> "Base to the power of exponent" {
        T: result = 1;
        for i in 0..exp {
            result *= base;
        }
        result
    }
    static T: factorial(T: val) -> "Factorial" {
        T: result = 1;
        for i in 1..val {
            result *= i;
        }
        result
    }
    static T: gcd(T: left, T: right) -> "Greatest Common Divisor" {
        while right != 0 {
            T: temp = right;
            right = left % right;
            left = temp;
        }
        left
    }
    static T: lcm(T: left, T: right) -> "Least Common Multiple" => left * right / gcd(left, right);
    static T: is_prime(T: val) -> "Check if number is prime" {
        if val < 2 {
            return false;
        }
        for i in 2..val {
            if val % i == 0 {
                return false;
            }
        }
        true
    }
    static T: is_even(T: val) -> "Check if number is even" => val % 2 == 0;
    static T: is_odd(T: val) -> "Check if number is odd" => val % 2 != 0;
    static T: is_positive(T: val) -> "Check if number is positive" => val > 0;
    static T: is_negative(T: val) -> "Check if number is negative" => val < 0;
    static T: is_zero(T: val) -> "Check if number is zero" => val == 0;
}

pub struct u8 <- Number, Equitable<u8, u16>, Equitable<u8, u32>, Equitable<u8, u64>, Equitable<u8, u128>, Equitable<u8, i8>, Equitable<u8, i16>, Equitable<u8, i32>, Equitable<u8, i64>, Equitable<u8, i128>, Equitable<u8, f32>, Equitable<u8, f64>, Default, ToString -> "Unsigned 8-bit integer" {
    const MIN: u8 = 0;
    const MAX: u8 = 255;

    priv u8: Value = 0; // will cause infinite loop

    pub u8: sin() => Math::sin(Value);
    pub u8: cos() => Math::cos(Value);
    pub u8: tan() => Math::tan(Value);
    pub u8: asin() => Math::asin(Value);
    pub u8: acos() => Math::acos(Value);
    pub u8: atan() => Math::atan(Value);
    pub u8: sinh() => Math::sinh(Value);
    pub u8: cosh() => Math::cosh(Value);
    pub u8: tanh() => Math::tanh(Value);
    pub u8: asinh() => Math::asinh(Value);
    pub u8: acosh() => Math::acosh(Value);
    pub u8: atanh() => Math::atanh(Value);
    pub u8: abs() => Math::abs(Value);
    pub u8: sq() => Math::sq(Value);
    pub u8: sqrt() => Math::sqrt(Value);
    pub u8: pow(u8: exp) => Math::pow(Value, exp);
    pub u8: factorial() => Math::factorial(Value);
    pub u8: is_prime() => Math::is_prime(Value);
    pub u8: is_even() => Math::is_even(Value);
    pub u8: is_odd() => Math::is_odd(Value);
    pub u8: is_positive() => Math::is_positive(Value);
    pub u8: is_negative() => Math::is_negative(Value);
    pub u8: is_zero() => Math::is_zero(Value);

    // implementing Numbers interface
    pub u8[]: to_bytes() => [Value];
    pub bool[]: to_bits() {
        bool: bits[8];
        for i in 0..8 {
            bits[i] = Value & (1 << i) != 0;
        }
        bits
    }
    pub u8: from_bits(bool[]: bits) {
        u8: result = 0;
        for i in 0..8 {
            result |= bits[i] ? 1 << i : 0;
        }
        result
    }

    priv static u8: to_u8<T # Number>(T: val) {
        val.to_u8();
    }

    pub u8: from(u16: val) => u8 { Value = to_u8<u16>(val) } 
    pub u8: from(u32: val) => u8 { Value = to_u8<u32>(val) } 
    pub u8: from(u64: val) => u8 { Value = to_u8<u64>(val) } 
    pub u8: from(u128: val) => u8 { Value = to_u8<u128>(val) } 
    pub u8: from(i8: val) => u8 { Value = to_u8<i8>(val) } 
    pub u8: from(i16: val) => u8 { Value = to_u8<i16>(val) } 
    pub u8: from(i32: val) => u8 { Value = to_u8<i32>(val) } 
    pub u8: from(i64: val) => u8 { Value = to_u8<i64>(val) } 
    pub u8: from(i128: val) => u8 { Value = to_u8<i128>(val) } 
    pub u8: from(f32: val) => u8 { Value = to_u8<f32>(val) } 
    pub u8: from(f64: val) => u8 { Value = to_u8<f64>(val) } 

    pub u8: equals(u16: val) => Value == val as u8;
    pub u8: equals(u32: val) => Value == val as u8;
    pub u8: equals(u64: val) => Value == val as u8;
    pub u8: equals(u128: val) => Value == val as u8;
    pub u8: equals(i8: val) => Value == val as u8;
    pub u8: equals(i16: val) => Value == val as u8;
    pub u8: equals(i32: val) => Value == val as u8;
    pub u8: equals(i64: val) => Value == val as u8;
    pub u8: equals(i128: val) => Value == val as u8;
    pub u8: equals(f32: val) => Value == val as u8;
    pub u8: equals(f64: val) => Value == val as u8;

    // implementing ToString interface 
    pub u8: to_string() => string::from_number<u8>(Value);

    // implementing Default interface
    static pub u8: default() => 0_u8;

    // implementing Equitabls interfaces
    pub u8: op_add(u8: val) => Value + val;
    pub u8: op_sub(u8: val) => Value - val;
    pub u8: op_mul(u8: val) => Value * val;
    pub u8: op_div(u8: val) => Value / val;
    pub u8: op_mod(u8: val) => Value % val;
    pub u8: op_pow(u8: val) => Value ** val;
    pub u8: op_inc() => Value++;
    pub u8: op_dec() => Value--;
    pub bool: op_gt(u8: val) => Value > val;
    pub bool: op_gteq(u8: val) => Value >= val;
    pub bool: op_lt(u8: val) => Value < val;
    pub bool: op_lteq(u8: val) => Value <= val;
    pub bool: op_eq(u8: val) => Value == val;  
    pub bool: op_neq(u8: val) => Value != val; 
    pub u8: op_and(u8: val) => Value & val;
    pub u8: op_or(u8: val) => Value | val;
    pub u8: op_xor(u8: val) => Value ^ val;
    pub u8: op_not() => !Value;
    pub u8: op_shl(u8: val) => Value << val;
    pub u8: op_shr(u8: val) => Value >> val;
    pub u8: op_neg() => -Value;
    pub u8: op_inv() => ~Value;
    
    // u8: val = var_i32 + var_f32 + var_u64
    // u8: val = u8::from(var_i32) + u8::from(var_f32) + u8::from(var_u64)
} 

// instead of the above, which causes an infinite loop, this might be the better option:
pub interface Number <- Default, ToString, NumericPrimitiveWrapper -> "Number Primitive Wrapper" {
    // Default interface
    static Number: default() -> "Default value for number '0'";
    // ToString interface
    Number: to_string(Number: val) -> "Convert number to string";

    static Self: from<T # NumericPrimitive>(T: val) -> "Convert numeric primitive type to another numeric primitive type";
    u8[]: to_bytes() -> "Convert number to byte array";
    bool[]: to_bits() -> "Convert number to bit array";

    Self: sin() -> "Sine of number";
    Self: cos() -> "Cosine of number";
    Self: tan() -> "Tangent of number";
    Self: abs() -> "Absolute value of number";
    Self: sq() -> "Square of number";
    Self: sqrt() -> "Square root of number";
    Self: pow(Self: exp) -> "Raise number to power";
    Self: factorial() -> "Factorial of number";
    bool: is_prime() -> "Check if number is prime";
    bool: is_even() -> "Check if number is even";
    bool: is_odd() -> "Check if number is odd";
    bool: is_positive() -> "Check if number is positive";
    bool: is_negative() -> "Check if number is negative";
    bool: is_zero() -> "Check if number is zero";
    
    u8: to_u8() -> "convert to type";
    u16: to_u16() -> "convert to type";
    u32: to_u32() -> "convert to type";
    u64: to_u64() -> "convert to type";
    u128: to_u128() -> "convert to type";
    i8: to_i8() -> "convert to type";
    i16: to_i16() -> "convert to type";
    i32: to_i32() -> "convert to type";
    i64: to_i64() -> "convert to type";
    i128: to_i128() -> "convert to type";
    f32: to_f32() -> "convert to type";
    f64: to_f64() -> "convert to type";
}

pub struct Int32 <- Number, NoOperationOverloads, NumericPrimitive<i32>, Equitable<Int32, u8>, Equitable<Int32, u16>, Equitable<Int32, u32>, Equitable<Int32, u64>, Equitable<Int32, u128>, Equitable<Int32, i8>, Equitable<Int32, i16>, Equitable<Int32, i64>, Equitable<Int32, i128>, Equitable<Int32, f32>, Equitable<Int32, f64>, Default, ToString -> "Signed 32-bit integer" {
    const MIN: Int32 = -2147483648;
    const MAX: Int32 = 2147483647;

    priv i32: inner_value = 0;

    // implementing Numbers interface

    i32: sin() => Math::sin(inner_value);
    i32: cos() => Math::cos(inner_value);
    i32: tan() => Math::tan(inner_value);
    i32: abs() => Math::abs(inner_value);
    i32: sq() => Math::sq(inner_value);
    i32: sqrt() => Math::sqrt(inner_value);
    i32: pow(i32: exp) => Math::pow(inner_value, exp);
    i32: factorial() => Math::factorial(inner_value);
    bool: is_prime() => Math::is_prime(inner_value);
    bool: is_even() => Math::is_even(inner_value);
    bool: is_odd() => Math::is_odd(inner_value);
    bool: is_positive() => Math::is_positive(inner_value);
    bool: is_negative() => Math::is_negative(inner_value);
    bool: is_zero() => Math::is_zero(inner_value);

    static i32: from<T # NumericPrimitive>(T: val) {
        
    }
    i32[]: to_bytes() {

    }
    bool[]: to_bits() {
        
    }

    static i32: to_i32<T # NumericPrimitive>(T: val) {
        from<T>(val);
    }

    u8: to_u8() => inner_value as u8;
    u16: to_u16() => inner_value as u16;
    u32: to_u32() => inner_value as u32;
    u64: to_u64() => inner_value as u64;
    u128: to_u128() => inner_value as u128;
    i8: to_i8() => inner_value as i8;
    i16: to_i16() => inner_value as i16;
    i32: to_i32() => inner_value as i32;
    i64: to_i64() => inner_value as i64;
    i128: to_i128() => inner_value as i128;
    f32: to_f32() => inner_value as f32;
    f64: to_f64() => inner_value as f64;
}
```