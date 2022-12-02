#include <stdio.h>
#include <stdlib.h>

int main(void) {
  char *line = NULL;
  size_t len = 0;
  ssize_t lineSize = 0;
  char *end;
  lineSize = getline(&line, &len, stdin);
  long i = strtol(line, &end, 10);
  printf("%d", i + 1123123);
  free(line);
  return 0;

}