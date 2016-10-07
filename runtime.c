#include <stdio.h>

int print_int(long n)
{
    return printf("%ld", n);
}

long read_int()
{
    long rc;
    scanf("%ld", &rc);
    return rc;
}
