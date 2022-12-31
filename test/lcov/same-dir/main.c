#include <stdlib.h>

void extra(int a, int b, int c);

int main(int argc, char* argv[])
{
  extra(4, 5, 6);
  if (argc > 2){
    return atoi(argv[1]) + atoi(argv[2]);
  }
  return 0;
}
