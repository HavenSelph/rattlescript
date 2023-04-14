//
// Created by haven on 4/13/2023.
//

#ifndef CALLUC_VALUE_H
#define CALLUC_VALUE_H

#include <stdio.h>
#include <stdlib.h>

typedef struct List List;

typedef enum ValueType {
    VT_Int,
    VT_Float,
    VT_String,
    VT_List
} ValueType;

typedef struct Value {
    int ref_count;
    ValueType type;
    union {
        int as_int;
        float as_float;
        char *as_string;
        List *as_list;
    };
} Value;

// Value constructors
Value *value_int(int);
Value *value_float(float);
Value *value_string(char*);
Value *value_list(int);

// Utility functions
void value_print(Value*);
void print_debug();

// Value reference counting
Value *value_ref(Value*);
void value_unref(Value*);

#endif //CALLUC_VALUE_H
