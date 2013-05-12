#include <stdlib.h>
#include <stdio.h>

int main() {
  printf("int_%i_%i_%i_%i\n", sizeof(short),sizeof(int),sizeof(long),sizeof(size_t));
  printf("flt_%i_%i\n", sizeof(float), sizeof(double));
}
