//
// Created by haven on 4/13/2023.
//

#ifndef CALLUC_LIST_H
#define CALLUC_LIST_H

#include <stdlib.h>
#include <stdio.h>
#include "value.h"

typedef struct List {
    Value **values;
    int length;
    int capacity;
} List;

List *list_new(int);

void list_append(List*, Value*);
void list_insert(List*, int, Value*);
Value list_pop(List*);

// Utility functions
void list_print(List*);

void list_free(List*);
void list_clear(List *list);


#endif //CALLUC_LIST_H
