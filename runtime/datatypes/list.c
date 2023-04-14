//
// Created by haven on 4/13/2023.
//

#include "list.h"
#include "common.h"

List *list_new(int capacity) {
    List *list = malloc(sizeof(List));
    list->values = malloc(sizeof(Value*) * capacity);
    list->length = 0;
    list->capacity = capacity;
    return list;
}

static void check_size(List *list) {
    if (list->length == list->capacity) {
        list->capacity *= 2;
        list->values = realloc(list->values, sizeof(Value*) * list->capacity);
    }
}

void list_append(List *list, Value *value) {
    check_size(list);
    list->values[list->length++] = value;
}

void list_insert(List *list, int index, Value *value) {
    if (index < 0 || index > list->length)
        error("Index out of bounds");
    check_size(list);
    for (int i = list->length; i > index; i--) {
        list->values[i] = list->values[i - 1];
    }
    list->values[index] = value;
    list->length++;
}

Value list_pop(List *list) {
    if (list->length == 0)
        error("Cannot pop from empty list");
    list->length--;
    return *list->values[list->length];
}

void list_print(List *list) {
    printf("[");
    for (int i = 0; i < list->length; i++) {
        if (i > 0) printf(", ");
        value_print(list->values[i]);
    }
    printf("]");
}

void list_clear(List *list) {
    for (int i = 0; i < list->length; i++) {
        value_unref(list->values[i]);
    }
    list->length = 0;
}

void list_free(List *list) {
    list_clear(list);
    free(list->values);
    free(list);
}
