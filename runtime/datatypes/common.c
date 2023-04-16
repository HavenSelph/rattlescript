//
// Created by haven on 4/13/2023.
//

#include "common.h"

__attribute__((noreturn))
void error(char *message) {
    printf("Error: %s", message);
    exit(1);
}
