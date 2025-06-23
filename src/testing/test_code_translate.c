#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#define DEFINE_VEC(type)                                            \
typedef struct {                                                    \
    type* data;                                                     \
    size_t size;                                                    \
    size_t capacity;                                                \
} Vec_lt_##type##_gt;                                                       \
Vec_lt_##type##_gt Vec_lt_##type##_gt_new(type* data, size_t length) {            \
    Vec_lt_##type##_gt v;                                                   \
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
type* Vec_lt_##type##_gt_get(Vec_lt_##type##_gt* v, size_t index) {               \
    if (index >= v->size) {                                         \
        fprintf(stderr, "Out of bounds\n");                         \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    return &v->data[index];                                         \
}                                                                   \
void Vec_lt_##type##_gt_push(Vec_lt_##type##_gt* v, type element) {               \
    if (v->size == v->capacity) {                                   \
        v->capacity *= 2;                                           \
        v->data = realloc(v->data, v->capacity * sizeof(type));     \
    }                                                               \
    v->data[v->size++] = element;                                   \
}                                                                   \
void Vec_lt_##type##_gt_free(Vec_lt_##type##_gt* v) {                             \
    free(v->data);                                                  \
    v->data = NULL;                                                 \
}                                                                   \
size_t Vec_lt_##type##_gt_len(Vec_lt_##type##_gt* v) {                            \
    return v->size;                                                 \
}


#define FOR_LOOP(set, condition, increment, expression) \
for (set; condition; increment) { \
    expression; \
}
#define FOR_LOOP_WITH_INDEX(index, set, condition, increment, expression) \
int index = 0; \
for (set; condition; increment) { \
    expression; \
    index++; \
}
#define REGULAR_FOR_LOOP(name, condition, expression) \
for (int name = 0; name < condition; name++) { \
    expression; \
}

#define FOR_EACH_LOOP_WITH_VEC(type, temp, item, arr, expression) \
for (int temp = 0; temp < Vec_lt_##type##_gt_len(&arr); temp++) { \
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
    char* data;
    size_t length;
} string;

string string_new(const char* literal) {
    size_t len = strlen(literal);
    char* data = malloc(len + 1);
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
    char* data = malloc(a.length + b.length + 1);
    memcpy(data, a.data, a.length);
    memcpy(data + a.length, b.data, b.length);
    data[a.length + b.length] = '\0';
    return (string){ data, a.length + b.length };
}

void string_add(string* dest, string src) {
    size_t new_len = dest->length + src.length;
    char* new_data = malloc(new_len + 1);

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
typedef char* char_ptr;
typedef float* float_ptr;
typedef unsigned int uint;

DEFINE_VEC(int);
DEFINE_TUPLE(tuple_lp_char_char_rp, char a; char b;);
DEFINE_VEC(tuple_lp_char_char_rp);
DEFINE_VEC(Vec_lt_tuple_lp_char_char_rp_gt);
DEFINE_VEC(char);
DEFINE_VEC(bool);
DEFINE_VEC(bool_ptr);

int hmm() {}
void use_array(Vec_lt_char_gt _) {}
int fancy_function(int a, int b, int c, Vec_lt_int_gt d) {}
int something() {}
void nothing() {}
Vec_lt_int_gt Vec_lt_int_gt_select__(Vec_lt_int_gt *a, int (*fn)(int)) {}
void main() {}

//ArrayExpression
// pub struct Vec<T> { Y: select<Y>(Fn<Y, T>: fn) where T = int, Y = char
Vec_lt_char_gt Vec_lt_int_gt_select_char(Vec_lt_int_gt *v, char (*fn)(int)) {
    VEC_LITERAL_VAR_EMPTY(result, char);
    for (int temp1 = 0; temp1 < Vec_lt_int_gt_len(v); temp1++) {
        int temp2 = v->data[temp1];
        char item = fn(temp2);
        Vec_lt_char_gt_push(&result, item);
    }
    return result;
}
char lambda1(int x) {
    return (char)x;
}
void select_test() {
    // i32[]: arr = [1, 2, 3, 4, 5];
    VEC_LITERAL_VAR(arr, int, 5, 1, 2, 3, 4, 5);
    // char[]: result = arr.select(x => x as char);
    Vec_lt_char_gt result = Vec_lt_int_gt_select_char(&arr, lambda1);

    // if another value needs this, it will need to clone it
    //some_ther_value = result.clone();

    // free(arr);
    Vec_lt_int_gt_free(&arr);
    Vec_lt_char_gt_free(&result);
}

Vec_lt_bool_gt* get_bool_array() {
    VEC_LITTERAL_PTR(temp1, bool, 3, false, true, false);
    return temp1;
}

void arrays() {
    // i32[]: arr = [];
    VEC_LITERAL_VAR_EMPTY(arr, int);
    // arr.push(99);
    Vec_lt_int_gt_push(&arr, 99);
    
    // (u8, u8)[][]: b = [[(0, 0)], [], [(1, 1)]];
    VEC_LITERAL_VAR(temp1, tuple_lp_char_char_rp, 1, (0, 0));
    VEC_LITERAL_VAR_EMPTY(temp2, tuple_lp_char_char_rp);
    VEC_LITERAL_VAR(temp3, tuple_lp_char_char_rp, 1, (1, 1));
    VEC_LITERAL_VAR(b, Vec_lt_tuple_lp_char_char_rp_gt, 3, temp1, temp2, temp3);
    
    // use_array([0, 1, 2])
    WITH_VEC(char, temp4, temp5, BRACE(0, 1, 2), 3, use_array(temp4));

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
    Vec_lt_int_gt_free(&arr);

    Vec_lt_Vec_lt_tuple_lp_char_char_rp_gt_gt_free(&b);
    Vec_lt_bool_ptr_gt_free(&bp_array);
}
//AsCast
void as_cast() {
    // i32: a = 0;
    int a = 0;
    // u8: c = a as u8;
    char c = (char)a;
    //f32*: arr = malloc(sizeof(f32) * 5) as f32*;
    float_ptr arr = (float_ptr)malloc(sizeof(float) * 5);
    // free(arr);
    free(arr);
}
//Assignment
void assignment() {
    // i32: a = 0;
    int a = 0;
    // a += 25;
    a = a + 25;
    // a = fancy_function(0, 1, 2, [9]);
    WITH_VEC(int, temp1, temp2, BRACE(9), 1, a = fancy_function(0, 1, 2, temp1));
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
int lambda2(int _) {
    return something();
}
void discard() {
    // _ = 0 + hmm();
    0 + hmm();
    // i32: arr = [1, 2, 3];
    // for _ in arr { nothing(); }
    WITH_VEC(int, arr, temp1, BRACE(1, 2, 3), 3, {
        for (int temp2 = 0; temp2 < Vec_lt_int_gt_len(&arr); temp2++) {
            nothing();
        }
        Vec_lt_int_gt_select__(&arr, lambda2);
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
    VEC_LITERAL_VAR_EMPTY(n, int);
    REGULAR_FOR_LOOP(i, 10, {
        FOR_LOOP(int j = 0, j < 10, j = j + 2, {
            Vec_lt_int_gt_push(&n, i * j);
        })
    });
    FOR_LOOP_WITH_INDEX(index, int i = Vec_lt_int_gt_len(&n) / 4, i < Vec_lt_int_gt_len(&n) * 0.75, i = i + index, {
        printf("%d", Vec_lt_int_gt_get(&n, i));        
    })
}
//ForEach
Vec_lt_tuple_lp_char_char_rp_gt* z() { 
    VEC_EMPTY_RETURN_PTR(tuple_lp_char_char_rp, temp1); 
}
Vec_lt_int_gt int_as_range(int* this) {
    return Vec_lt_int_gt_new(NULL, *this);
}
void for_each_loop() {
    WITH_VEC(bool, temp1, temp2, BRACE(true, false, false, true), 4, {
        FOR_EACH_LOOP_WITH_VEC(bool, temp3, i, temp1, {
            printf("%d", i);
        });
    });
    
    Vec_lt_tuple_lp_char_char_rp_gt* temp4 = z();
    FOR_EACH_LOOP_WITH_VEC(tuple_lp_char_char_rp, temp5, as_tuple_x_y, *temp4, {
        printf("%d", as_tuple_x_y->a);
        printf("%d", as_tuple_x_y->b);
    });
    Vec_lt_tuple_lp_char_char_rp_gt_free(temp4);

    int collection = 100;
    Vec_lt_int_gt temp6 = int_as_range(&collection);
    FOR_EACH_LOOP_WITH_VEC(int, i, item, temp6, {
        printf("%d", i);
    });
    Vec_lt_int_gt_free(&temp6);
}
//FunctionCall
#define DEFINE_GENERICS_3_2(T, G, F) \
T generics_lt_##T##_##G##_##F##_gt(G g, F f) { \
    T result = (T)g; \
    result = result + (T)f; \
    return result; \
}
DEFINE_GENERICS_3_2(int, float, uint)
int add(int a, int b) {
    return a + b;
}
void function_call() {
    int result = add(1, 2);
    printf("%d", result);
    Vec_lt_tuple_lp_char_char_rp_gt* aaa = z();
    int bbb = generics_lt_int_float_uint_gt(3.14, 42);
    Vec_lt_tuple_lp_char_char_rp_gt_free(aaa);
}
//FunctionDeclaration
#define DEFINE_PARSE_1_1(T) T parse_lt_##T##_rt(string s) {}
DEFINE_PARSE_1_1(int)

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

DEFINE_LOT_3_1(int, float, string);

void function_declaration() {
    STRING_LIFETIME(temp, string_new("123"), 
        int a = parse_lt_int_rt(temp);
    );

    STRING_LIFETIME(temp2, string_new("string"), 
        GET_TUPLE_VAR(tuple_lp_int_string_rp, temp3, 25, temp2);
        Vec_lt_tuple_lp_int_float_rp_gt_ptr b = lot_lt_int_float_string_gt(temp3);
        Vec_lt_tuple_lp_int_float_rp_gt_free(b);
    );
}
//Identifier
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