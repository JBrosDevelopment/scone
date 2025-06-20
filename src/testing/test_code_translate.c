#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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

    

//ArrayExpression
DEFINE_VEC(int);
DEFINE_TUPLE(tuple_char_char, char a; char b;);
DEFINE_VEC(tuple_char_char);
DEFINE_VEC(Vec_tuple_char_char);
DEFINE_VEC(char);
void main() {
    VEC_LITERAL_VAR_EMPTY(arr, int);
    Vec_int_push(&arr, 99);
    debug_output_arr(arr);
    
    VEC_LITERAL_VAR(temp1, tuple_char_char, 1, (0, 0));
    VEC_LITERAL_VAR_EMPTY(temp2, tuple_char_char);
    VEC_LITERAL_VAR(temp3, tuple_char_char, 1, (1, 1));
    VEC_LITERAL_VAR(b, Vec_tuple_char_char, 3, temp1, temp2, temp3);
    debug_output_b(b);
    
    WITH_VEC(char, temp4, temp5, BRACE(0, 1, 2), 3, use_array(temp4));

    Vec_int_free(&arr);
    Vec_Vec_tuple_char_char_free(&b);
}
//AsCast
//Assignment
//Break
//ClassDeclaration
//CodeBlock
//Constant
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