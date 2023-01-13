#include <stdlib.h>
#include <stdio.h>
#include <errno.h>

void *checkNull(const void *ptr, const char *description) {
  if (ptr == NULL) {
    perror(description);
    exit(1);
  }

  return (void *) ptr;
}

unsigned checkErrCode(unsigned return_code, const char *description) {
  if (return_code != 0) {
    puts(description);
    exit(1);
  }

  return return_code;
}
