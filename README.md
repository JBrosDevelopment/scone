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

self is what I'm thinking the VM IR code might look like
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
```