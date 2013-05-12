#include <stdio.h>

/* need to flush stdout explicitly if Fortran argument function used.
   Easier this way, hence a C printing routine is provided. */
void pp1(void *xp)
{
    printf ("%12g", *((double *) xp));
}
