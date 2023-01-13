#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <dirent.h>
#include <errno.h>
#include <sys/stat.h>
#include "../utils.h"

#define FRAME_BUFFER_SIZE 3000
#define FRAME_LINES 22
#define ARGS_SIZE 230
#define PIECE_COUNT 7


#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define RESET "\x1B[m"

unsigned verbose = 0;

char coreInputs[ARGS_SIZE];
char coreArgs[ARGS_SIZE + 30];
char *bucket;
char renderER[PIECE_COUNT][FRAME_BUFFER_SIZE];
char nextStepER[PIECE_COUNT][ARGS_SIZE];



// compare strings, but when meet '?' in s2 - skip check equality appropriate chars.
// compare strings, but when meet '$' in s2 - skip check equality all next chars up to '\n' in s1.
// '$' - wildcard for chars to next line
// '?' - wildcard for one char
int strcmpWithWildcard(const char *s1, const char *s2) {
  unsigned i;
  unsigned j;
  unsigned skip;
  for (i = 0, j = 0, skip = 0; *(s1 + i) && *(s2 + j);) {
    if (skip) {
      if (*(s1 + i) == '\n') {
        skip = 0;
      } else {
        i++;
      }
      continue;
    }

    if (*(s2 + j) == '?') {
      i++;
      j++;
      continue;
    }

    if (*(s2 + j) == '$') {
      skip = 1;
      j++;
      continue;
    }

    if (*(s1 + i) == *(s2 + j)) {
      i++;
      j++;
    } else {
      break;
    }

  }

  return !(*(s1 + i) == *(s2 + j));
}

void run(const char* corePath) {
  FILE *corePipe;
  char actualNextStepResult[ARGS_SIZE];
  char actualRenderResult[FRAME_BUFFER_SIZE];
  char line[255];

  snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, "INIT_STATE");
  for (int i = 0; i < PIECE_COUNT; ++i) {

    corePipe = checkNull(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of next inputs
    fgets(actualNextStepResult, ARGS_SIZE, corePipe);

    // read lines from core - actual result of render
    bucket = actualRenderResult;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      bucket = stpcpy(bucket, line);
    }
    // test next args
    unsigned cur = 0;
    while (strcmpWithWildcard(actualNextStepResult, nextStepER[cur]) && cur < 7) {
      cur++;
    }
    if (cur == 7) {
      printf("%s - %sFailed%s\n", "INIT_STATE", RED, RESET);
      printf("Actual Result:\n%s\n", actualNextStepResult);
      exit(1);
    } else {
      printf("%s:%d - %sPassed%s\n", "INIT_STATE", cur, GREEN, RESET);
      if (verbose) printf("Actual Result:\n%s\n", actualNextStepResult);
    }
    int result = strcmpWithWildcard(actualRenderResult, renderER[cur]);
    if (result == 0) {
      printf("%s - %sPassed%s\n", "INIT_STATE", GREEN, RESET);
      if (verbose) printf("%s\n", actualRenderResult);
    } else {
      printf("%s - %sFailed%s\n", "INIT_STATE", RED, RESET);
      printf("strlen(actualRenderResult) - %lu   strlen(renderER[cur]) - %lu\n", strlen(actualRenderResult), strlen(renderER[cur]));
      printf("Actual Result:\n%s\n", actualRenderResult);
      printf("Expected Result:\n%s\n\n", renderER[cur]);
      printf("%s%s%s\n", RED, "FAIL", RESET);
      exit(1);
    }
    fclose(corePipe);
  }

}

void readER(FILE *fp) {
  char line[255];
  for (int i = 0; i < PIECE_COUNT; ++i) {
    fgets(line, 255, fp);
    stpcpy(nextStepER[i], line);
    bucket = renderER[i];
    for (int j = 0; j < FRAME_LINES; ++j) {
      fgets(line, 255, fp);
      bucket = stpcpy(bucket, line);
    }

    //read empty line 
    fgets(line, 255, fp);
  }
}

int main(int argc, char **argv) {
  if (argc < 3) {
    printf("%s\n", "provide arguments");
    return 1;
  }

  if (argc == 4 && argv[3][0] == '-' && argv[3][1] == 'v') {
    verbose = 1;
  }

  FILE *erFP = checkNull(fopen(argv[1], "r"), argv[1]);
  readER(erFP);
  run(argv[2]);
  // printf("%s%s%s\n", GREEN, "SUCCESS", RESET);
  return EXIT_SUCCESS;
}
