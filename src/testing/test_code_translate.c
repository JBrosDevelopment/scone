#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

typedef long i64;
typedef unsigned long u64;
typedef int i32;
typedef unsigned int u32;
typedef short i16;
typedef unsigned short u16;
typedef signed char i8;
typedef char u8;
typedef float f32;
typedef double f64;


#define DEFINE_VEC(type)                                            \
typedef struct {                                                    \
    type* data;                                                     \
    size_t size;                                                    \
    size_t capacity;                                                \
} Vec_lt_##type##_gt;                                               \
Vec_lt_##type##_gt Vec_lt_##type##_gt_new(type* data, size_t length) {  \
    Vec_lt_##type##_gt v;                                           \
    v.size = length;                                                \
    v.capacity = (length > 0) ? length : 1;                         \
    v.data = malloc(sizeof(type) * v.capacity);                     \
    if (!v.data) {                                                  \
        fprintf(stderr, "Memory allocation failed\n");              \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    memcpy(v.data, data, sizeof(type) * length);                    \
    return v;                                                       \
}                                                                   \
type* Vec_lt_##type##_gt_get(Vec_lt_##type##_gt* v, size_t index) { \
    if (index >= v->size) {                                         \
        fprintf(stderr, "Out of bounds\n");                         \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    return &v->data[index];                                         \
}                                                                   \
void Vec_lt_##type##_gt_push(Vec_lt_##type##_gt* v, type element) { \
    if (v->size == v->capacity) {                                   \
        v->capacity *= 2;                                           \
        v->data = realloc(v->data, v->capacity * sizeof(type));     \
    }                                                               \
    v->data[v->size++] = element;                                   \
}                                                                   \
void Vec_lt_##type##_gt_free(Vec_lt_##type##_gt* v) {               \
    free(v->data);                                                  \
    v->data = NULL;                                                 \
}                                                                   \
size_t Vec_lt_##type##_gt_len(Vec_lt_##type##_gt* v) {              \
    return v->size;                                                 \
}


#define FOR_LOOP(set, condition, increment, expression) \
for (set; condition; increment) { \
    expression; \
}
#define FOR_LOOP_WITH_INDEX(index, set, condition, increment, expression) \
i32 index = 0; \
for (set; condition; increment) { \
    expression; \
    index++; \
}
#define REGULAR_FOR_LOOP(name, condition, expression) \
for (i32 name = 0; name < condition; name++) { \
    expression; \
}

#define FOR_EACH_LOOP_WITH_VEC(type, temp, item, arr, expression) \
for (i32 temp = 0; temp < Vec_lt_##type##_gt_len(&arr); temp++) { \
    type* item = Vec_lt_##type##_gt_get(&arr, temp); \
    expression; \
}

#define CONCAT(x, y) x##y

#define COUNT_ARGS_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N
#define COUNT_ARGS(...) COUNT_ARGS_IMPL(__VA_ARGS__,10,9,8,7,6,5,4,3,2,1)
#define BRACE(...) { __VA_ARGS__ }

#define DEFINE_TUPLE(name, ...) \
    typedef struct { __VA_ARGS__ } name;

#define VEC_LITERAL_VAR(name, T, LEN, ...) \
    T temp_##name##_arr[] = { __VA_ARGS__ }; \
    Vec_lt_##T##_gt name = Vec_lt_##T##_gt_new(temp_##name##_arr, LEN);

#define VEC_LITERAL_VAR_EMPTY(name, T) \
    Vec_lt_##T##_gt name = Vec_lt_##T##_gt_new(NULL, 0);

#define VEC_EMPTY_RETURN_PTR(type, temp) \
    Vec_lt_##type##_gt* temp = malloc(sizeof(Vec_lt_##type##_gt)); \
    *temp = Vec_lt_##type##_gt_new(NULL, 0); \
    return temp; 

#define WITH_VEC(TYPE, NAME, TEMP_NAME, VALUES, LEN, CALL_EXPR) \
    TYPE TEMP_NAME[] = VALUES; \
    Vec_lt_##TYPE##_gt NAME = Vec_lt_##TYPE##_gt_new(TEMP_NAME, LEN); \
    CALL_EXPR; \
    Vec_lt_##TYPE##_gt_free(&NAME); 

#define VEC_LITTERAL_PTR(name, T, LEN, ...) \
    T temp_##name##_arr[] = { __VA_ARGS__ }; \
    Vec_lt_##T##_gt* name = malloc(sizeof(Vec_lt_##T##_gt)); \
    *name = Vec_lt_##T##_gt_new(temp_##name##_arr, LEN);

#define GET_TUPLE_VAR(type, name, ...) \
    type name = (type){ __VA_ARGS__ }; 

typedef struct _String {
    u8* data;
    size_t length;
} string;

string string_new(const u8* literal) {
    size_t len = strlen(literal);
    u8* data = malloc(len + 1);
    memcpy(data, literal, len + 1); // includes null terminator
    return (string){ data, len };
}

string string_clone(string s) {
    return string_new(s.data);
}

void string_free(string* s) {
    free(s->data);
    s->data = NULL;
    s->length = 0;
}

string string_concat(string a, string b) {
    u8* data = malloc(a.length + b.length + 1);
    memcpy(data, a.data, a.length);
    memcpy(data + a.length, b.data, b.length);
    data[a.length + b.length] = '\0';
    return (string){ data, a.length + b.length };
}

void string_add(string* dest, string src) {
    size_t new_len = dest->length + src.length;
    u8* new_data = malloc(new_len + 1);

    memcpy(new_data, dest->data, dest->length);
    memcpy(new_data + dest->length, src.data, src.length);
    new_data[new_len] = '\0';

    free(dest->data); // free old buffer
    dest->data = new_data;
    dest->length = new_len;
}
    
bool string_equals(string a, string b) {
    if (a.length != b.length) return false;
    return memcmp(a.data, b.data, a.length) == 0;
}

#define USE_STRING(temp, constant, expression) \
    string temp = string_new(constant); \
    expression; \
    string_free(&temp);

#define STRING_LIFETIME(name, value, expression) \
    string name = value; \
    expression; \
    string_free(&name);

#define STRING_LIFETIME_RETURNS(name, value, expression) \
    string name = value; \
    expression; 



typedef bool* bool_ptr;
typedef u8* u8_ptr;
typedef f32* f32_ptr;

DEFINE_VEC(i32);
DEFINE_TUPLE(tuple_lp_u8_u8_rp, u8 a; u8 b;);
DEFINE_VEC(tuple_lp_u8_u8_rp);
DEFINE_VEC(Vec_lt_tuple_lp_u8_u8_rp_gt);
DEFINE_VEC(u8);
DEFINE_VEC(bool);
DEFINE_VEC(bool_ptr);

i32 hmm() {}
void use_array(Vec_lt_u8_gt _) {}
i32 fancy_function(i32 a, i32 b, i32 c, Vec_lt_i32_gt d) {}
i32 something() {}
void nothing() {}
Vec_lt_i32_gt Vec_lt_i32_gt_select__(Vec_lt_i32_gt *a, i32 (*fn)(i32)) {}
void main() {}

//ArrayExpression
// pub struct Vec<T> { Y: select<Y>(Fn<Y, T>: fn) where T = i32, Y = u8
Vec_lt_u8_gt Vec_lt_i32_gt_select_u8(Vec_lt_i32_gt *v, u8 (*fn)(i32)) {
    VEC_LITERAL_VAR_EMPTY(result, u8);
    for (i32 temp1 = 0; temp1 < Vec_lt_i32_gt_len(v); temp1++) {
        i32 temp2 = v->data[temp1];
        u8 item = fn(temp2);
        Vec_lt_u8_gt_push(&result, item);
    }
    return result;
}
u8 lambda1(i32 x) {
    return (u8)x;
}
void select_test() {
    // i32[]: arr = [1, 2, 3, 4, 5];
    VEC_LITERAL_VAR(arr, i32, 5, 1, 2, 3, 4, 5);
    // u8[]: result = arr.select(x => x as u8);
    Vec_lt_u8_gt result = Vec_lt_i32_gt_select_u8(&arr, lambda1);

    // if another value needs this, it will need to clone it
    //some_ther_value = result.clone();

    // free(arr);
    Vec_lt_i32_gt_free(&arr);
    Vec_lt_u8_gt_free(&result);
}

Vec_lt_bool_gt* get_bool_array() {
    VEC_LITTERAL_PTR(temp1, bool, 3, false, true, false);
    return temp1;
}

void arrays() {
    // i32[]: arr = [];
    VEC_LITERAL_VAR_EMPTY(arr, i32);
    // arr.push(99);
    Vec_lt_i32_gt_push(&arr, 99);
    
    // (u8, u8)[][]: b = [[(0, 0)], [], [(1, 1)]];
    VEC_LITERAL_VAR(temp1, tuple_lp_u8_u8_rp, 1, (0, 0));
    VEC_LITERAL_VAR_EMPTY(temp2, tuple_lp_u8_u8_rp);
    VEC_LITERAL_VAR(temp3, tuple_lp_u8_u8_rp, 1, (1, 1));
    VEC_LITERAL_VAR(b, Vec_lt_tuple_lp_u8_u8_rp_gt, 3, temp1, temp2, temp3);
    
    // use_array([0, 1, 2])
    WITH_VEC(u8, temp4, temp5, BRACE(0, 1, 2), 3, use_array(temp4));

    // bool: val = false; 
    bool val = false;
    // bool*[]: bp_array: [&val]; 
    VEC_LITERAL_VAR(bp_array, bool_ptr, 1, &val);
    //bool*[]*: bp_array_ptr = &bp_array;
    Vec_lt_bool_ptr_gt* bp_array_ptr = &bp_array;
    // free(bp_array_ptr);
    Vec_lt_bool_ptr_gt_free(bp_array_ptr);

    // bool[]: bools = get_bool_array();
    Vec_lt_bool_gt* bools = get_bool_array();
    FOR_EACH_LOOP_WITH_VEC(bool, temp6, i, *bools, printf("%d", b));

    Vec_lt_bool_gt_free(bools);
    Vec_lt_i32_gt_free(&arr);

    Vec_lt_Vec_lt_tuple_lp_u8_u8_rp_gt_gt_free(&b);
    Vec_lt_bool_ptr_gt_free(&bp_array);
}
//AsCast
void as_cast() {
    // i32: a = 0;
    i32 a = 0;
    // u8: c = a as u8;
    u8 c = (u8)a;
    //f32*: arr = malloc(sizeof(f32) * 5) as f32*;
    f32_ptr arr = (f32_ptr)malloc(sizeof(f32) * 5);
    // free(arr);
    free(arr);
}
//Assignment
void assignment() {
    // i32: a = 0;
    i32 a = 0;
    // a += 25;
    a = a + 25;
    // a = fancy_function(0, 1, 2, [9]);
    WITH_VEC(i32, temp1, temp2, BRACE(9), 1, a = fancy_function(0, 1, 2, temp1));
}
//Break
//ClassDeclaration
//CodeBlock
//Constant
string func(string name) {
    // string: s = "hello, " + name;
    USE_STRING(temp1, "hello, ", 
    STRING_LIFETIME_RETURNS(s, string_concat(temp1, name), 
    USE_STRING(temp2, "!",
    // s += "!";
    string_add(&s, name);
    )));
    // return s;
    return s;
}

void constants() {
    // string: s = func("John");
    USE_STRING(temp1, "John", 
        STRING_LIFETIME(s, func(temp1), 
            // printf("%d", s);
            printf("%s\n", s.data);
        )
    );
}
//Continue
//Discard
i32 lambda2(i32 _) {
    return something();
}
void discard() {
    // _ = 0 + hmm();
    0 + hmm();
    // i32: arr = [1, 2, 3];
    // for _ in arr { nothing(); }
    WITH_VEC(i32, arr, temp1, BRACE(1, 2, 3), 3, {
        for (i32 temp2 = 0; temp2 < Vec_lt_i32_gt_len(&arr); temp2++) {
            nothing();
        }
        Vec_lt_i32_gt_select__(&arr, lambda2);
    });
}
//EnumDeclaration
typedef enum {
    Options_Yes,
    Options_No,
    Options_Maybe,
} Options;
typedef enum {
    Attributes_ThisTihng = 0b00,
    Attributes_ThatThing = 0b01,
    Attributes_OtherThing = 0b10,
} Attributes;
//For
void for_loop() {
    VEC_LITERAL_VAR_EMPTY(n, i32);
    REGULAR_FOR_LOOP(i, 10, {
        FOR_LOOP(i32 j = 0, j < 10, j = j + 2, {
            Vec_lt_i32_gt_push(&n, i * j);
        })
    });
    FOR_LOOP_WITH_INDEX(index, i32 i = Vec_lt_i32_gt_len(&n) / 4, i < Vec_lt_i32_gt_len(&n) * 0.75, i = i + index, {
        printf("%d", Vec_lt_i32_gt_get(&n, i));        
    })
}
//ForEach
Vec_lt_tuple_lp_u8_u8_rp_gt* z() { 
    VEC_EMPTY_RETURN_PTR(tuple_lp_u8_u8_rp, temp1); 
}
Vec_lt_i32_gt i32_as_range(i32* this) {
    return Vec_lt_i32_gt_new(NULL, *this);
}
void for_each_loop() {
    WITH_VEC(bool, temp1, temp2, BRACE(true, false, false, true), 4, {
        FOR_EACH_LOOP_WITH_VEC(bool, temp3, i, temp1, {
            printf("%d", i);
        });
    });
    
    Vec_lt_tuple_lp_u8_u8_rp_gt* temp4 = z();
    FOR_EACH_LOOP_WITH_VEC(tuple_lp_u8_u8_rp, temp5, as_tuple_x_y, *temp4, {
        printf("%d", as_tuple_x_y->a);
        printf("%d", as_tuple_x_y->b);
    });
    Vec_lt_tuple_lp_u8_u8_rp_gt_free(temp4);

    i32 collection = 100;
    Vec_lt_i32_gt temp6 = i32_as_range(&collection);
    FOR_EACH_LOOP_WITH_VEC(i32, i, item, temp6, {
        printf("%d", i);
    });
    Vec_lt_i32_gt_free(&temp6);
}
//FunctionCall
#define DEFINE_GENERICS_3_2(T, G, F) \
T generics_lt_##T##_##G##_##F##_gt(G g, F f) { \
    T result = (T)g; \
    result = result + (T)f; \
    return result; \
}
DEFINE_GENERICS_3_2(i32, f32, u32)
i32 add(i32 a, i32 b) {
    return a + b;
}
void function_call() {
    i32 result = add(1, 2);
    printf("%d", result);
    Vec_lt_tuple_lp_u8_u8_rp_gt* aaa = z();
    i32 bbb = generics_lt_i32_f32_u32_gt(3.14, 42);
    Vec_lt_tuple_lp_u8_u8_rp_gt_free(aaa);
}
//FunctionDeclaration
#define DEFINE_PARSE_1_1(T) T parse_lt_##T##_rt(string s) {}
DEFINE_PARSE_1_1(i32)

#define DEFINE_LOT_3_1(T, G, Q) \
DEFINE_TUPLE(tuple_lp_##T##_##Q##_rp, T a; Q b;); \
DEFINE_TUPLE(tuple_lp_##T##_##G##_rp, T a; G b;); \
typedef tuple_lp_##T##_##G##_rp* tuple_lp_##T##_##G##_rp_ptr; \
DEFINE_VEC(tuple_lp_##T##_##G##_rp); \
typedef Vec_lt_tuple_lp_##T##_##G##_rp_gt* Vec_lt_tuple_lp_##T##_##G##_rp_gt_ptr; \
Vec_lt_tuple_lp_##T##_##G##_rp_gt_ptr lot_lt_##T##_##G##_##Q##_gt(tuple_lp_##T##_##Q##_rp g) { \
    Vec_lt_tuple_lp_##T##_##G##_rp_gt_ptr result = (Vec_lt_tuple_lp_##T##_##G##_rp_gt_ptr)malloc(sizeof(T) + sizeof(G)); \
    tuple_lp_##T##_##G##_rp_ptr temp1 = Vec_lt_tuple_lp_##T##_##G##_rp_gt_get(result, 0); \
    temp1->a = g.a; \
    return result; \
};

DEFINE_LOT_3_1(i32, f32, string);

void function_declaration() {
    STRING_LIFETIME(temp, string_new("123"), 
        i32 a = parse_lt_i32_rt(temp);
    );

    STRING_LIFETIME(temp2, string_new("string"), 
        GET_TUPLE_VAR(tuple_lp_i32_string_rp, temp3, 25, temp2);
        Vec_lt_tuple_lp_i32_f32_rp_gt_ptr b = lot_lt_i32_f32_string_gt(temp3);
        Vec_lt_tuple_lp_i32_f32_rp_gt_free(b);
    );
}
//Identifier
void identifier() {
    // i32: a = 0;
    i32 var_0 = 0;
    // f32: b = 1.5;
    f32 var_1 = 1.5;
    // u32: c = a + b;
    u32 var_2 = var_0 + var_1;
    // printf("a: %d, b: %f, c: %u", a, b, c);
    printf("a: %d, b: %f, c: %u\n", var_0, var_1, var_2);

}
//Indexer
//If
//IsCheck
//LambdaExpression
//Match
//ObjectInstantiation
//Operator
//ReturnExpression
//ScopedExpression
//Shebang
//StructDeclaration
//TernaryOperator
//TraitDeclaration
//TupleDeclaration
//TupleExpression
//TypeDef
//TypeDefinition
//TypeIdentifier
//UnaryOperator
//Use
//VariableDeclaration
//While
//DeferStatement