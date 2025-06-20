#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdbool.h>

#define DEFINE_VEC(type)                                            \
typedef struct _Vector_##type {                                     \
    type* data;                                                     \
    size_t size;                                                    \
    size_t capacity;                                                \
} Vec_##type;                                                       \
Vec_##type Vec_##type##_new(type* data, size_t length) {            \
    Vec_##type v;                                                   \
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
type* Vec_##type##_get(Vec_##type* v, size_t index) {               \
    if (index >= v->size) {                                         \
        fprintf(stderr, "Out of bounds\n");                         \
        exit(EXIT_FAILURE);                                         \
    }                                                               \
    return &v->data[index];                                         \
}                                                                   \
void Vec_##type##_push(Vec_##type* v, type element) {               \
    if (v->size == v->capacity) {                                   \
        v->capacity *= 2;                                           \
        v->data = realloc(v->data, v->capacity * sizeof(type));     \
    }                                                               \
    v->data[v->size++] = element;                                   \
}                                                                   \
void Vec_##type##_free(Vec_##type* v) {                             \
    free(v->data);                                                  \
    v->data = NULL;                                                 \
}

#define CONCAT(x, y) x##y

#define COUNT_ARGS_IMPL(_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,N,...) N
#define COUNT_ARGS(...) COUNT_ARGS_IMPL(__VA_ARGS__,10,9,8,7,6,5,4,3,2,1)
#define BRACE(...) { __VA_ARGS__ }

#define DEFINE_TUPLE(name, ...) \
    typedef struct _Struct_##name { __VA_ARGS__ } name;

#define VEC_LITERAL_VAR(name, T, LEN, ...) \
    T temp_##name##_arr[] = { __VA_ARGS__ }; \
    Vec_##T name = Vec_##T##_new(temp_##name##_arr, LEN);

#define VEC_LITERAL_VAR_EMPTY(name, T) \
    Vec_##T name = Vec_##T##_new(NULL, 0);

#define WITH_VEC(TYPE, NAME, TEMP_NAME, VALUES, LEN, CALL_EXPR) \
    do { \
    TYPE TEMP_NAME[] = VALUES; \
        Vec_##TYPE NAME = Vec_##TYPE##_new(TEMP_NAME, LEN); \
        CALL_EXPR; \
        Vec_##TYPE##_free(&NAME); \
    } while (0)


typedef void (*BoxDestructor)(void*);

typedef struct {
    void* data;
    size_t size;
    BoxDestructor destructor;
} Box;

Box box_new_copy(const void* value, size_t size, BoxDestructor destructor) {
    void* copy = malloc(size);
    memcpy(copy, value, size);
    return (Box){ .data = copy, .size = size, .destructor = destructor };
}

Box box_from_ptr(void* ptr, size_t size, BoxDestructor destructor) {
    return (Box){ .data = ptr, .size = size, .destructor = destructor };
}

void box_free(Box* box) {
    if (box->destructor) {
        box->destructor(box->data);
    } else {
        free(box->data);
    }
    box->data = NULL;
    box->size = 0;
    box->destructor = NULL;
}

#define BOX_GET(box, Type) ((Type*)(box).data)
#define BOX_SCOPE(name, value, size, dtor) \
    Box name = box_new_copy((value), (size), (dtor)); \
    __attribute__((cleanup(box_free))); Box* _##name##_auto_free = &name;


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
    

typedef bool* bool_ptr;
typedef char* char_ptr;
typedef float* float_ptr;

//ArrayExpression
DEFINE_VEC(int);
DEFINE_TUPLE(tuple_char_char, char a; char b;);
DEFINE_VEC(tuple_char_char);
DEFINE_VEC(Vec_tuple_char_char);
DEFINE_VEC(char);
DEFINE_VEC(bool_ptr);

void arrays() {
    // i32[]: arr = [];
    VEC_LITERAL_VAR_EMPTY(arr, int);
    // arr.push(99);
    Vec_int_push(&arr, 99);
    
    // (u8, u8)[][]: b = [[(0, 0)], [], [(1, 1)]];
    VEC_LITERAL_VAR(temp1, tuple_char_char, 1, (0, 0));
    VEC_LITERAL_VAR_EMPTY(temp2, tuple_char_char);
    VEC_LITERAL_VAR(temp3, tuple_char_char, 1, (1, 1));
    VEC_LITERAL_VAR(b, Vec_tuple_char_char, 3, temp1, temp2, temp3);
    
    // use_array([0, 1, 2])
    WITH_VEC(char, temp4, temp5, BRACE(0, 1, 2), 3, use_array(temp4));

    // bool: val = false; 
    bool val = false;
    // bool*[]: bp_array: [&val]; 
    VEC_LITERAL_VAR(bp_array, bool_ptr, 1, &val);
    //bool*[]*: bp_array_ptr = &bp_array;
    Vec_bool_ptr* bp_array_ptr = &bp_array;
    // free(bp_array_ptr);
    free(bp_array_ptr);

    Vec_int_free(&arr);
    Vec_Vec_tuple_char_char_free(&b);
    Vec_bool_ptr_free(&bp_array);
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
//Break                 - transpiled directly
//ClassDeclaration      - not supported
//CodeBlock             - transpiled directly
//Constant
void constants() {
    // string: s = func("John");
    string temp1 = string_new("John");
    string s = func(temp1);
    string_free(&temp1);
    // print(s);
    printf("%s\n", s.data);

    string_free(&s);
    
}
string func(string name) {
    // string: s = "hello, " + name;
    string s = string_new("hello, ");
    string_add(&s, name);
    // s += "!";
    string temp1 = string_new("!");
    string_add(&s, temp1);
    // return s;
    string_free(&temp1);
    return s;
}

//Continue
//Discard
//EnumDeclaration
//EnumDeclaration
//For
//ForEach
//FunctionCall
//FunctionDeclaration
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