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
void Vec_lt_##type##_gt_set(Vec_lt_##type##_gt* v, size_t index, type value) { \
    if (index >= v->size) {                                         \
        fprintf(stderr, "Out of bounds\n");                         \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    v->data[index] = value;                                         \
}                                                                   \
i32 Vec_lt_##type##_gt_index_of_cmp(Vec_lt_##type##_gt* v, type element, bool (*cmp)(type, type)) { \
    for (size_t i = 0; i < v->size; i++) {                          \
        if (cmp(v->data[i], element)) {                             \
            return (i32)i;                                          \
        }                                                           \
    }                                                               \
    return -1;                                                      \
}                                                                   \
void Vec_lt_##type##_gt_push(Vec_lt_##type##_gt* v, type element) { \
    if (v->size >= v->capacity) {                                   \
        v->capacity *= 2;                                           \
        v->data = realloc(v->data, v->capacity * sizeof(type));     \
    }                                                               \
    v->data[v->size++] = element;                                   \
}                                                                   \
void Vec_lt_##type##_gt_free(Vec_lt_##type##_gt* v) {               \
    /*if T is string { for s in Self { if s != NULL { s.free();}}}*/\
    free(v->data);                                                  \
    v->data = NULL;                                                 \
}                                                                   \
size_t Vec_lt_##type##_gt_len(Vec_lt_##type##_gt* v) {              \
    return v->size;                                                 \
}                                                                   \
type Vec_lt_##type##_gt_op_index_get(Vec_lt_##type##_gt* v, size_t index) { \
    return *Vec_lt_##type##_gt_get(v, index);                       \
}                                                                   \
Vec_lt_##type##_gt Vec_lt_##type##_gt_clone(Vec_lt_##type##_gt* v) { \
    Vec_lt_##type##_gt clone;                                       \
    clone.size = v->size;                                           \
    clone.capacity = v->capacity;                                   \
    clone.data = malloc(sizeof(type) * v->capacity);                \
    if (!clone.data) {                                              \
        fprintf(stderr, "Memory allocation failed in clone\n");     \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    memcpy(clone.data, v->data, sizeof(type) * v->size);            \
    return clone;                                                   \
}

#define DEFINE_VEC_SELECT(T, Y) \
Vec_lt_##Y##_gt Vec_lt_##T##_gt_select_##Y(Vec_lt_##T##_gt *v, Y (*fn)(T)) { \
    VEC_LITERAL_VAR_EMPTY(result, Y); \
    for (size_t temp1 = 0; temp1 < Vec_lt_##T##_gt_len(v); temp1++) { \
        T temp2 = v->data[temp1]; \
        Y item = fn(temp2); \
        Vec_lt_##Y##_gt_push(&result, item); \
    } \
    return result; \
}

#define DEFINE_VEC_SELECT_INDEX(T, Y) \
Vec_lt_##Y##_gt Vec_lt_##T##_gt_select_##Y##_index(Vec_lt_##T##_gt *v, Y (*fn)(T, i32)) { \
    VEC_LITERAL_VAR_EMPTY(result, Y); \
    for (size_t temp1 = 0; temp1 < Vec_lt_##T##_gt_len(v); temp1++) { \
        T temp2 = v->data[temp1]; \
        Y item = fn(temp2, temp1); \
        Vec_lt_##Y##_gt_push(&result, item); \
    } \
    return result; \
}

#define DEFINE_VEC_WHERE(T) \
Vec_lt_##T##_gt Vec_lt_##T##_gt_where(Vec_lt_##T##_gt *v, bool (*fn)(T)) { \
    VEC_LITERAL_VAR_EMPTY(result, T); \
    for (size_t temp1 = 0; temp1 < Vec_lt_##T##_gt_len(v); temp1++) { \
        T temp2 = v->data[temp1]; \
        bool temp3 = fn(temp2); \
        if (temp2) { \
            Vec_lt_##T##_gt_push(&result, temp2); \
        } \
    } \
    return result; \
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

#define FOR_EACH_LOOP(type, temp, item, arr, expression) \
for (i32 temp = 0; temp < Vec_lt_##type##_gt_len(&arr); temp++) { \
    type item = Vec_lt_##type##_gt_op_index_get(&arr, temp); \
    expression; \
}

#define MATCH_CASE(condition, expression) \
    case condition: \
        expression; \
        break;
#define MATCH_DEFAULT(expression) \
    default: \
        expression; \
        break;

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

#define VEC_LIFETIME_AUTO(TYPE, NAME, TEMP_NAME, VALUES, LEN, CALL_EXPR) \
    TYPE TEMP_NAME[] = VALUES; \
    Vec_lt_##TYPE##_gt NAME = Vec_lt_##TYPE##_gt_new(TEMP_NAME, LEN); \
    CALL_EXPR; \
    Vec_lt_##TYPE##_gt_free(&NAME); 

#define VEC_LIFETIME(TYPE, NAME, VALUE, CALL_EXPR) \
    Vec_lt_##TYPE##_gt NAME = VALUE; \
    CALL_EXPR; \
    Vec_lt_##TYPE##_gt_free(&NAME); 

#define VEC_LITERAL_PTR(name, T, LEN, ...) \
    T temp_##name##_arr[] = { __VA_ARGS__ }; \
    Vec_lt_##T##_gt* name = malloc(sizeof(Vec_lt_##T##_gt)); \
    *name = Vec_lt_##T##_gt_new(temp_##name##_arr, LEN);

#define VEC_FROM_FUNCTION(TYPE, NAME, FUNCTION, EXPRESSION) \
    Vec_lt_##TYPE##_gt_ptr NAME = FUNCTION; \
    EXPRESSION; \
    Vec_lt_##TYPE##_gt_free(NAME); 

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

//ArrayExpression
DEFINE_VEC_SELECT(i32, u8);
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
    VEC_LITERAL_PTR(temp1, bool, 3, false, true, false);
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
    VEC_LIFETIME_AUTO(u8, temp4, temp5, BRACE(0, 1, 2), 3, use_array(temp4));

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
    FOR_EACH_LOOP(bool, temp6, b, *bools, printf("%d\n", b));

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
    VEC_LIFETIME_AUTO(i32, temp1, temp2, BRACE(9), 1, a = fancy_function(0, 1, 2, temp1));
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
            // printf("%d\n", s);
            printf("%s\n", s.data);
        )
    );
}
//Continue
//DeferStatement
void defer_statement() {
    i32* ptr = malloc(sizeof(i32));
    *ptr = 42;
    printf("%d\n", *ptr);
    // defer free(ptr);
    free(ptr);
}
//Discard
i32 lambda2(i32 _) {
    return something();
}
void discard() {
    // _ = 0 + hmm();
    0 + hmm();
    // i32: arr = [1, 2, 3];
    // for _ in arr { nothing(); }
    VEC_LIFETIME_AUTO(i32, arr, temp1, BRACE(1, 2, 3), 3, {
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
    Attributes_ThisThing = 0b00,
    Attributes_ThatThing = 0b01,
    Attributes_OtherThing = 0b10,
} Attributes;
void enums() {
    Options option = Options_Yes;
    switch (option) {
        MATCH_CASE(Options_Yes, printf("Yes\n"))
        MATCH_CASE(Options_No, printf("No\n"))
        MATCH_CASE(Options_Maybe, printf("Maybe\n"))
    }

    Attributes attr = Attributes_ThisThing | Attributes_ThatThing;
    if ((attr & Attributes_ThisThing) != 0) {
        printf("ThisThing is set\n");
    }
}
//For
void for_loop() {
    VEC_LITERAL_VAR_EMPTY(n, i32);
    REGULAR_FOR_LOOP(i, 10, {
        FOR_LOOP(i32 j = 0, j < 10, j = j + 2, {
            Vec_lt_i32_gt_push(&n, i * j);
        })
    });
    FOR_LOOP_WITH_INDEX(index, i32 i = Vec_lt_i32_gt_len(&n) / 4, i < Vec_lt_i32_gt_len(&n) * 0.75, i = i + index, {
        printf("%d\n", *Vec_lt_i32_gt_get(&n, i));        
    })
}
//ForEach
Vec_lt_tuple_lp_u8_u8_rp_gt* z() { 
    VEC_EMPTY_RETURN_PTR(tuple_lp_u8_u8_rp, temp1); 
}
Vec_lt_i32_gt i32_as_range(i32* this) {
    i32 count = *this;
    i32* data = malloc(sizeof(i32) * count);
    for (i32 i = 0; i < count; i++) {
        data[i] = i;
    }
    return Vec_lt_i32_gt_new(data, count);
}

void for_each_loop() {
    VEC_LIFETIME_AUTO(bool, temp1, temp2, BRACE(true, false, false, true), 4, {
        FOR_EACH_LOOP(bool, temp3, i, temp1, {
            printf("%d\n", i);
        });
    });
    
    Vec_lt_tuple_lp_u8_u8_rp_gt* temp4 = z();
    FOR_EACH_LOOP(tuple_lp_u8_u8_rp, temp5, as_tuple_x_y, *temp4, {
        printf("%d\n", as_tuple_x_y.a);
        printf("%d\n", as_tuple_x_y.b);
    });
    Vec_lt_tuple_lp_u8_u8_rp_gt_free(temp4);

    i32 collection = 100;
    Vec_lt_i32_gt temp6 = i32_as_range(&collection);
    FOR_EACH_LOOP(i32, i, item, temp6, {
        printf("%d\n", i);
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
    printf("%d\n", result);
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
Vec_lt_tuple_lp_##T##_##G##_rp_gt_ptr lot_lt_##T##_##G##_##Q##_gt(tuple_lp_##T##_##Q##_rp g) {}

DEFINE_LOT_3_1(i32, f32, string);

void function_declaration() {
    STRING_LIFETIME(temp1, string_new("123"), 
        i32 a = parse_lt_i32_rt(temp1);
    );

    STRING_LIFETIME(temp2, string_new("string"),
        GET_TUPLE_VAR(tuple_lp_i32_string_rp, temp3, 25, temp2);
        Vec_lt_tuple_lp_i32_f32_rp_gt_ptr b = lot_lt_i32_f32_string_gt(temp3);
        // Vec_lt_tuple_lp_i32_f32_rp_gt_free(b); // commented out to avoid segment fault because real "lot" function isn't defined
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
DEFINE_VEC(string)
bool i32_cmp(i32 a, i32 b) {
    return a == b;
}
#define DEFINE_STRUCT_MAP(name, K, V, COMPARE) \
typedef struct { \
    Vec_lt_##K##_gt keys; \
    Vec_lt_##V##_gt values; \
} name##_lt_##K##_##V##_gt; \
V name##_lt_##K##_##V##_op_indexer(name##_lt_##K##_##V##_gt *self, K key) { \
    i32 index = Vec_lt_##K##_gt_index_of_cmp(&self->keys, key, COMPARE); \
    if (index < 0) { \
        fprintf(stderr, "Key not found\n"); \
        exit(EXIT_FAILURE); \
    } \
    return Vec_lt_##V##_gt_op_index_get(&self->values, index); \
} \
void name##_lt_##K##_##V##_op_index_set(name##_lt_##K##_##V##_gt *self, K key, V value) { \
    i32 index = Vec_lt_##K##_gt_index_of_cmp(&self->keys, key, COMPARE); \
    if (index == -1) { \
        Vec_lt_##K##_gt_push(&self->keys, key); \
        Vec_lt_##V##_gt_push(&self->values, value); \
        return; \
    } \
    Vec_lt_##V##_gt_set(&self->values, index, value); \
} \
name##_lt_##K##_##V##_gt name##_lt_##K##_##V##_gt_new() { \
    name##_lt_##K##_##V##_gt map; \
    map.keys = Vec_lt_##K##_gt_new(NULL, 0); \
    map.values = Vec_lt_##V##_gt_new(NULL, 0); \
    return map; \
}

DEFINE_STRUCT_MAP(Map, string, i32, string_equals);

void indexer() {
    // i32[]: arr = [1, 2, 3, 4, 5];
    VEC_LIFETIME_AUTO(i32, arr, temp1, BRACE(1, 2, 3, 4, 5), 5, 
    // i32: value = arr[2];
    i32 value = *Vec_lt_i32_gt_get(&arr, 2);
    // arr[0] = 10;
    Vec_lt_i32_gt_set(&arr, 0, 10);
    // printf("%d\n", value);
    );

    // Map<string, i32>: map = Map::new<string, i32>();
    Map_lt_string_i32_gt map = Map_lt_string_i32_gt_new();
    // map["key"] = 32;
    STRING_LIFETIME(temp2, string_new("key"),
    Map_lt_string_i32_op_index_set(&map, temp2, 32);
    );
}
//If
void if_statement() {
    i32 a = 10;

    if (a > 5) {
        printf("a is greater than 5\n");
    } else if (a < 5) {
        printf("a is less than 5\n");
    } else {
        printf("a is equal to 5\n");
    }
}
//IsCheck
void is_check() {
    i32 a = 10;
    if (true) { // Is handled by the compiler: a is i32 -> true
        printf("a is an i32\n");
    }
}
//LambdaExpression
DEFINE_VEC_SELECT(i32, i32);
DEFINE_VEC_SELECT_INDEX(i32, u8);
DEFINE_VEC_WHERE(u8);
i32 lambda3(i32 x) {
    return x * x;
}
i32 double_lambda(i32 x, i32(*fn)(i32)) {
    return fn(x) * 2;
}
i32 lambda4(i32 x) {
    return x + 1;
}
u8 lambda5(i32 x, i32 y) {
    return x + 48 - y;
}
bool lambda6(u8 x) {
    return x >= '0' && x <= '9';
}
void lambda() {
    VEC_LIFETIME_AUTO(i32, arr, temp1, BRACE(1, 2, 3, 4, 5), 5,
    VEC_LIFETIME(i32, temp2, Vec_lt_i32_gt_select_i32(&arr, lambda3), 
    VEC_LIFETIME(i32, squared, Vec_lt_i32_gt_clone(&temp2), 
        FOR_EACH_LOOP(i32, temp3, item, squared, 
            printf("%d\n", item);
        )
        double_lambda(10, lambda4);
        
    VEC_LIFETIME(u8, temp5, Vec_lt_i32_gt_select_u8_index(&squared, lambda5), 
    VEC_LIFETIME(u8, temp6, Vec_lt_u8_gt_where(&temp5, lambda6), 
    u8 ch = *Vec_lt_u8_gt_get(&temp6, 0);
    printf("%c\n", ch);
    );
    );
    );
    );
    );
}
//Match
DEFINE_TUPLE(tuple_lp_string_i32_rp, string a; i32 b;);
void match_statement() {
    i32 a = 10;
    switch (a)
    {
        MATCH_CASE(0, printf("a is zero\n");)
        MATCH_CASE(1, printf("a is one\n");)
        MATCH_CASE(2, printf("a is two\n");)
        MATCH_DEFAULT(printf("a is something else\n");)
    }

    STRING_LIFETIME(temp1, string_new("hello"),
    STRING_LIFETIME(temp2, string_new("bye"),
    GET_TUPLE_VAR(tuple_lp_string_i32_rp, pair, temp1, 42);
    GET_TUPLE_VAR(tuple_lp_string_i32_rp, temp3, temp1, 42);
    GET_TUPLE_VAR(tuple_lp_string_i32_rp, temp4, temp1, 0);
    if (string_equals(pair.a, temp3.a) && pair.b == temp3.b) {
        printf("Matched hello and 42\n");
    } else if (string_equals(pair.a, temp4.a) && pair.b == temp4.b) {
        printf("Matched bye and 0 again\n");
    } else {
        printf("No match found\n");
    }
    );
    );
}
//ObjectInstantiation
typedef struct {
    f32 x;
    f32 y;
} Position;
Position position_new(f32 x, f32 y) {
    Position p;
    p.x = x;
    p.y = y;
    return p;
}
void position_add(Position* p, Position other) {
    p->x += other.x;
    p->y += other.y;
}
void object_instantiation() {
    Position p = position_new(1, 2);
    Position temp1;
    temp1.x = 1;
    temp1.y = 5;
    position_add(&p, temp1);
    printf("Position (%d, %d)\n", p.x, p.y);
}
//Operator
string i32_to_string(i32 value) {
    // convert i32 to string
    return string_new("42"); // placeholder implementation
}
void operator() {
    i32 a = 5 * 2;
    i32 b = a + 3;

    STRING_LIFETIME(temp1, string_new("Hello, "), 
    STRING_LIFETIME(temp2, string_new("World!"),
    STRING_LIFETIME(str, string_concat(temp1, temp2),
    STRING_LIFETIME(temp3, i32_to_string(b),
    if (string_equals(str, temp3) || a > b) {
        printf("Stuff\n");
    }
    );
    );
    );
    );
}
//ReturnExpression
string return_string() {
    // return "Hello, World!";
    return string_new("Hello, World!");
}
string return_string_with_param(string p) {
    // string: s = "Hello, World!";
    STRING_LIFETIME_RETURNS(s, string_new("Hello, World!"),
    // s += p;
    string_add(&s, p);
    return s;
    );
}
typedef Vec_lt_string_gt* Vec_lt_string_gt_ptr;
Vec_lt_string_gt_ptr return_string_array() {
    // return ["Hello", "World"];
    VEC_LITERAL_PTR(temp, string, 2, string_new("Hello"), string_new("World"));
    return temp;
}
i32 return_int() {
    // return 42;
    return 42;
}
void return_expression() {
    STRING_LIFETIME(result, return_string(), 
    STRING_LIFETIME(temp1, string_new(" from Scone!"), 
    STRING_LIFETIME(result_with_param, return_string_with_param(temp1), 
    VEC_FROM_FUNCTION(string, temp2, return_string_array(), 
    VEC_LIFETIME(string, result_array, Vec_lt_string_gt_clone(temp2), 
    i32 result_int = return_int();
    
    printf("%s\n", result.data);
    printf("%s\n", result_with_param.data);
    FOR_EACH_LOOP(string, temp3, item, result_array, 
        printf("%s\n", item.data);
    );
    printf("%d\n", result_int);
    );
    );
    );
    );
    );
}
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




int main() {
    printf("ARRAYS: \n");
    arrays();
    printf("\nSELECT: \n");
    select_test();
    printf("\nAS_CAST: \n");
    as_cast();
    printf("\nASSIGNMENT: \n");
    assignment();
    printf("\nCONSTANTS: \n");
    constants();
    printf("\nDEFER: \n");
    defer_statement();
    printf("\nDISCARD: \n");
    discard();
    printf("\nENUMS: \n");
    enums();
    printf("\nFOR: \n");
    for_loop();
    printf("\nFOR_EACH: \n");
    for_each_loop();
    printf("\nFUNCTION_CALL: \n");
    function_call();
    printf("\nFUNCTION_DECLARATION: \n");
    function_declaration();
    printf("\nIDENTIFIER: \n");
    identifier();
    printf("\nINDEXER: \n");
    indexer();
    printf("\nIF: \n");
    if_statement();
    printf("\nIS_CHECK: \n");
    is_check();
    printf("\nLAMBDA: \n");
    lambda();
    printf("\nMATCH: \n");
    match_statement();
    printf("\nOBJECT_INSTANTIATION: \n");
    object_instantiation();
    printf("\nOPERATOR: \n");
    operator();
    printf("\nRETURN_EXPRESSION: \n");
    return_expression();

    return 0;
}