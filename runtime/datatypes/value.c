//
// Created by haven on 4/13/2023.
//

#include <string.h>
#include <stdarg.h>
#include <math.h>
#include "common.h"
#include "value.h"
#include "list.h"

static int value_alloc_count = 0;
static int value_free_count = 0;

static Value *value_new(ValueType type) {
    Value *value = malloc(sizeof(Value));
    value->ref_count = 1;
    value->type = type;
    value_alloc_count++;
    return value;
}


Value *value_int(int val) {
    Value *value = value_new(VT_Int);
    value->as_int = val;
    return value;
}

Value *value_float(float val) {
    Value *value = value_new(VT_Float);
    value->as_float = val;
    return value;
}

Value *value_bool(bool val) {
    Value *value = value_new(VT_Bool);
    value->as_bool = val;
    return value;
}

Value *value_string(char *val) {
    // Handles string copy
    Value *value = value_new(VT_String);
    value->as_string = strdup(val);
    return value;
}

static Value *value_string_no_dup(char *val) {
    // Does not handle string copy
    Value *value = value_new(VT_String);
    value->as_string = val;
    return value;
}

Value *value_list(int capacity) {
    Value *value = value_new(VT_List);
    value->as_list = list_new(capacity);
    return value;
}

Value *value_list_v(int size, ...) {
    Value *value = value_new(VT_List);
    value->as_list = list_new(size);
    va_list args;
    va_start(args, size);
    for (int i = 0; i < size; i++) {
        list_append(value->as_list, va_arg(args, Value*));
    }
    va_end(args);
    return value;
}

Value *value_replace(Value *value, Value *new_value) {
    value_unref(value);
    return new_value;
}

void value_print(Value *value) {
    switch (value->type) {
        case VT_Int:
            printf("%d", value->as_int);
            break;
        case VT_Float:
            printf("%f", value->as_float);
            break;
        case VT_String:
            printf("%s", value->as_string);
            break;
        case VT_List:
            list_print(value->as_list);
            break;
        case VT_Bool:
            printf("%s", value->as_bool ? "true" : "false");
            break;
    }
}

__attribute__((destructor))
void print_debug() {
    printf("Value alloc count: %d\n", value_alloc_count);
    printf("Value free count: %d\n", value_free_count);
}

Value *value_add(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int(left->as_int + right->as_int);
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(left->as_int + right->as_float);
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out = value_float(left->as_float + right->as_int);
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(left->as_float + right->as_float);
    } else if (left->type == VT_String && right->type == VT_String) {
        char *new_string = malloc(strlen(left->as_string) + strlen(right->as_string) + 1);
        strcpy(new_string, left->as_string);
        strcat(new_string, right->as_string);
        out = value_string_no_dup(new_string);
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for addition");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_sub(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int(left->as_int - right->as_int);
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(left->as_int - right->as_float);
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out =  value_float(left->as_float - right->as_int);
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(left->as_float - right->as_float);
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for subtraction");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_mul(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int(left->as_int * right->as_int);
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(left->as_int * right->as_float);
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out = value_float(left->as_float * right->as_int);
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(left->as_float * right->as_float);
    } else if (left->type == VT_String && right->type == VT_Int) {
        char *new_string = malloc(strlen(left->as_string) * right->as_int + 1);
        strcpy(new_string, left->as_string);
        for (int i = 1; i < right->as_int; i++) {
            strcat(new_string, left->as_string);
        }
        out = value_string_no_dup(new_string);
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for multiplication");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_div(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int(left->as_int / right->as_int);
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(left->as_int / right->as_float);
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out = value_float(left->as_float / right->as_int);
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(left->as_float / right->as_float);
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for division");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_mod(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int(left->as_int % right->as_int);
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(fmod(left->as_int, right->as_float));
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out = value_float(fmod(left->as_float, right->as_int));
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(fmod(left->as_float, right->as_float));
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for modulo");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_pow(Value *left, Value *right) {
    Value *out;
    if (left->type == VT_Int && right->type == VT_Int) {
        out = value_int((int)pow(left->as_int, right->as_int));
    } else if (left->type == VT_Int && right->type == VT_Float) {
        out = value_float(pow( left->as_int, right->as_float));
    } else if (left->type == VT_Float && right->type == VT_Int) {
        out = value_float(powf(left->as_float, right->as_int));
    } else if (left->type == VT_Float && right->type == VT_Float) {
        out = value_float(powf(left->as_float, right->as_float));
    } else {
        value_unref(left); value_unref(right);
        error("Invalid types for exponentiation");
    }
    value_unref(left); value_unref(right);
    return out;
}

Value *value_as_bool(Value *value) {
    return value_bool(value_as_c_bool(value));
}

bool value_as_c_bool(Value *value) {
    bool out;
    if (value->type == VT_Int) {
        out = (value->as_int != 0);
    } else if (value->type == VT_Float) {
        out = (value->as_float != 0);
    } else if (value->type == VT_String) {
        out = (value->as_string[0] != '\0');
    } else if (value->type == VT_List) {
        out = (value->as_list->length != 0);
    } else if (value->type == VT_Bool) {
        out = (value->as_bool);
    } else {
        value_unref(value);
        error("Invalid type for boolean conversion");
    }
    value_unref(value);
    return out;
}

Value *value_not(Value *value) {
    Value *out = value_as_bool(value);
    out->as_bool = !out->as_bool;
    return out;
}

static void value_free(Value *value) {
    switch (value->type) {
        case VT_String:
            free(value->as_string);
            break;
        case VT_List:
            list_free(value->as_list);
            break;
        default:  // Handle types that require extra steps.
            break;
    }
    free(value);
    value_free_count++;
}

Value *value_ref(Value *value) {
    value->ref_count++;
    return value;
}

void value_unref(Value *value) {
    if (value->ref_count == 1) {
        value_free(value);
    } else {
        value->ref_count--;
    }
}

