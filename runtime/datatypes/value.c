//
// Created by haven on 4/13/2023.
//

#include <string.h>
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

Value *value_string(char *val) {
    // Handles string copy
    Value *value = value_new(VT_String);
    value->as_string = strdup(val);
    return value;
}

Value *value_list(int capacity) {
    Value *value = value_new(VT_List);
    value->as_list = list_new(capacity);
    return value;
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
    }
}

__attribute__((destructor))
void print_debug() {
    printf("Value alloc count: %d\n", value_alloc_count);
    printf("Value free count: %d\n", value_free_count);
}

void value_free(Value *value) {
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
