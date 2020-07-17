// Compile with:
// gcc-10 -O3 -march=native -ffast-math -funroll-loops test.c
#include "stdio.h"

int main()
{
    int i = 0;
    long int s = 0;
    while (i < 1000000000) {
        if (i % 2 == 0) s += i;
        i++;
    }
    printf("%ld\n", s);
}