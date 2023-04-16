//
// Created by haven on 4/13/2023.
//

#ifndef CALLUC_VALUE_H
#define CALLUC_VALUE_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct List List;

typedef enum ValueType {
    VT_Int,
    VT_Float,
    VT_Bool,
    VT_String,
    VT_List,
} ValueType;

typedef struct Value {
    int ref_count;
    ValueType type;
    union {
        int as_int;
        float as_float;
        bool as_bool;
        char *as_string;
        List *as_list;
    };
} Value;

// Value constructors
Value *value_int(int);
Value *value_float(float);
Value *value_bool(bool);
Value *value_string(char*);
Value *value_list(int);
Value *value_list_v(int, ...);

// Value replacement
Value *value_replace(Value*, Value*);

// Utility functions
void value_print(Value*);
void print_debug();

// Operations
Value *value_add(Value*, Value*);
Value *value_sub(Value*, Value*);
Value *value_mul(Value*, Value*);
Value *value_div(Value*, Value*);
Value *value_mod(Value*, Value*);
Value *value_pow(Value*, Value*);

// Logic
Value *value_as_bool(Value*);
bool value_as_c_bool(Value*);
Value *value_not(Value*);

// Value reference counting
Value *value_ref(Value*);
void value_unref(Value*);

#endif //CALLUC_VALUE_H
