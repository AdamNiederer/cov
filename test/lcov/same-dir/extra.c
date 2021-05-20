#include <stdio.h>

void extra(int a, int b, int c)
{
  for(int i = 0; i < a; ++i)
    for(int j = 0; j < b; ++j)
      for(int k = 0; k < c; ++k)
        printf("%x%x%x\n", i, j, k);
}
