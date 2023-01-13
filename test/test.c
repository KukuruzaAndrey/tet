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
#define ALL_PEICE_MASK ((~(1 << PIECE_COUNT)) & 0xFF)

#define RED "\033[31m"
#define GREEN "\033[32m"
#define YELLOW "\033[33m"
#define RESET "\x1B[m"

#define ERR_GEN 1
unsigned verbose = 0;

char coreInputs[ARGS_SIZE];
char coreArgs[ARGS_SIZE + 30];
char *bucket;

int (*pstrcmp)(const char *, const char *);


int compareResults(const char *actual, const char *expected, const char *testCase, const char *info, int (*pstrcmp)(const char *, const char *)) {
  int result = pstrcmp(actual, expected);
  if (result == 0) {
    printf("%s - %sPassed%s\n", info, GREEN, RESET);
  } else {
    printf("%s - %sFailed%s\n", info, RED, RESET);
    printf("strlen(actual) - %lu   strlen(expected) - %lu\n", strlen(actual), strlen(expected));
    printf("Case:\n%s\n", testCase);
    printf("Actual Result:\n%s\n", actual);
    printf("Expected Result:\n%s\n\n", expected);
  }

  return result;
}

// compare strings, but when meet '?' in s2 - skip check equality appropriate chars.
// '?' - wildcard for one char
int strcmpWithWildcard(const char *s1, const char *s2) {
  unsigned i;
  for (i = 0; *(s1 + i) && *(s2 + i) && ((*(s2 + i) == '?') || (*(s1 + i) == *(s2 + i))); i++) {}
  return !(*(s1 + i) == *(s2 + i) || *(s2 + i) == '?');
}

// compare strings, but when meet '$' in s2 - skip check equality all next chars up to '\n' in s1.
// '$' - wildcard for chars to next line
int strcmpWithSkip(const char *s1, const char *s2) {
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

int runMain(const char *testFileName, const char *corePath) {
  FILE *corePipe;
  FILE *testFile;
  char actualNextStepResult[ARGS_SIZE];
  char expectedNextStepResult[ARGS_SIZE];
  char actualRenderResult[FRAME_BUFFER_SIZE];
  char expectedRenderResult[FRAME_BUFFER_SIZE];
  char line[255];
  char caseInfo[255];
  unsigned file_line = 0;
  int rc = 0;

  // $ in file name marks that this file contains test with not 100% predictably
  // so we must use wildcards to skip not predictably parts
  const unsigned useWildcard = strchr(testFileName, '$') != NULL;

  // set default comparator
  pstrcmp = strcmp;

  // open file with test cases
  testFile = checkNull(fopen(testFileName, "r"), testFileName);

  // read arguments for test
  while (fgets(coreInputs, ARGS_SIZE, testFile) != NULL) {
    file_line += 1;

    // concatenate path to core and args
    snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);

    // open core with args
    corePipe = checkNull(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of next inputs
    fgets(actualNextStepResult, ARGS_SIZE, corePipe);

    // read lines from core - actual result of render
    bucket = actualRenderResult;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      bucket = stpcpy(bucket, line);
    }

    // read lines from case - expected result of next inputs
    fgets(expectedNextStepResult, ARGS_SIZE, testFile);

    // read lines from case - expected result of render
    bucket = expectedRenderResult;
    for (int j = 0; j < FRAME_LINES; ++j) {
      fgets(line, sizeof(line), testFile);
      bucket = stpcpy(bucket, line);
    }
    // if testcase with wildcards - use appropriate compare func
    if (useWildcard) {
      pstrcmp = strcmpWithWildcard;
    }

    snprintf(caseInfo, sizeof(caseInfo), "%s:%d", testFileName, file_line);
    // test next args
    int result = compareResults(actualNextStepResult, expectedNextStepResult, coreInputs, caseInfo, pstrcmp);
    if (result != 0) {
      rc = ERR_GEN;
      break;
    }

    // if testcase with wildcards - use appropriate compare func
    if (useWildcard) {
      pstrcmp = strcmpWithSkip;
    }
    // test render
    result = compareResults(actualRenderResult, expectedRenderResult, coreInputs, caseInfo, pstrcmp);

    if (result != 0) {
      rc = ERR_GEN;
      break;
    }
    // read empty line
    fgets(line, 255, testFile);

    file_line += 2 + FRAME_LINES;

    fclose(corePipe);
  }

  fclose(testFile);
  return rc;
}

int traverseAndExec(const char *dirPath, int (*exec)(const char *, const char *), const char *corePath) {
  // printf("entry %s\n", dirPath);
  int rc = 0;
  DIR *d = checkNull(opendir(dirPath), dirPath);
  struct dirent *entry;
  errno = 0;
  char pathToSubDir[1000];
  while ((entry = readdir(d)) != NULL) {
    snprintf(pathToSubDir, sizeof(pathToSubDir), "%s/%s", dirPath, entry->d_name);
    switch (entry->d_type) {
      case DT_REG:
        // printf("%s%s%s\n", YELLOW, pathToSubDir, RESET);
        rc = exec(pathToSubDir, corePath);
        // printf("ret=%d\n", ret);
        break;
      case DT_DIR:
        if (strcmp(entry->d_name, ".") != 0 && strcmp(entry->d_name, "..") != 0) {
          rc = traverseAndExec(pathToSubDir, exec, corePath);
        }
        break;
      default:
        break;
    }
    if (rc != 0) {
      break;
    }
  }

  if (errno != 0) {
    perror(dirPath);
    rc = ERR_GEN;
  }

  closedir(d);
  // printf("leave %s\n", dirPath);
  return rc;
}

int isDirectory(const char *path) {
  struct stat statbuf;
  if (stat(path, &statbuf) != 0) {
    return 0;
  }
  return S_ISDIR(statbuf.st_mode);
}

// compare strings, but when meet '?' in s2 - skip check equality appropriate chars.
// compare strings, but when meet '$' in s2 - skip check equality all next chars up to '\n' in s1.
// '$' - wildcard for chars to next line
// '?' - wildcard for one char
int strcmpWithWildcardAndSkip(const char *s1, const char *s2) {
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

int runInit(const char* corePath, char initNextStepER[PIECE_COUNT][ARGS_SIZE], char initRenderER[PIECE_COUNT][FRAME_BUFFER_SIZE]) {
  FILE *corePipe;
  char actualNextStepResult[ARGS_SIZE];
  char actualRenderResult[FRAME_BUFFER_SIZE];
  char line[255];
  char caseInfo[255];
  int rc = 0;
  snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, "INIT_STATE");

  unsigned piece_mask = 0;
  unsigned try_count = 0;
  // check if generates all pieces. and genarates properly
  while (1) {

    corePipe = checkNull(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of next inputs
    fgets(actualNextStepResult, ARGS_SIZE, corePipe);

    // read lines from core - actual result of render
    bucket = actualRenderResult;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      bucket = stpcpy(bucket, line);
    }
    fclose(corePipe);
    // test next args
    unsigned piece_number = actualNextStepResult[203] - '0'; //203 index of piece number
    // printf("piece_number %d\n", piece_number);
    if (!(piece_number >= 0 && piece_number <= PIECE_COUNT)) {
      printf("%s - %sFailed%s\n", "Incorrect piece number", RED, RESET);
      return ERR_GEN;
    }

    snprintf(caseInfo, sizeof(caseInfo), "%s:%d", "INIT_STATE", piece_number);
    rc = compareResults(actualNextStepResult, initNextStepER[piece_number], coreInputs, caseInfo, strcmpWithWildcardAndSkip);
    if (rc != 0) {
      return ERR_GEN;
    }

    rc = compareResults(actualRenderResult, initRenderER[piece_number], coreInputs, caseInfo, strcmpWithWildcardAndSkip);

    piece_mask |= 1 << piece_number;

    if (piece_mask == ALL_PEICE_MASK) {
      return 0;
    }

    try_count += 1;

    if (try_count >= 100) {
      printf("%s - %sFailed%s\n", "After %d iterations don't reach all kind of pieces. Possible broken generation.", RED, RESET);
      return ERR_GEN;
    }
  }

  return 0;
}

int runEnd(const char *testFileName, const char *corePath) {
  FILE *corePipe;
  FILE *testFile;
  char actualNextStepResult[ARGS_SIZE];
  char expectedNextStepResult[ARGS_SIZE];
  char actualRenderResult[FRAME_BUFFER_SIZE];
  char expectedRenderResult[FRAME_BUFFER_SIZE];
  char line[255];
  char caseInfo[255];
  int rc = 0;
  // open file with test cases
  testFile = checkNull(fopen(testFileName, "r"), testFileName);
  unsigned file_line = 0;
  // read arguments for test
  while (fgets(coreInputs, ARGS_SIZE, testFile) != NULL) {
    file_line += 1;

    // concatenate path to core and args
    snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);

    // open core with args
    corePipe = checkNull(popen(coreArgs, "r"), coreArgs);

    // read lines from core - actual result of next inputs
    fgets(actualNextStepResult, ARGS_SIZE, corePipe);

    // read lines from core - actual result of render
    bucket = actualRenderResult;
    while (fgets(line, sizeof(line), corePipe) != NULL) {
      bucket = stpcpy(bucket, line);
    }

    // read lines from case - expected result of next inputs
    fgets(expectedNextStepResult, ARGS_SIZE, testFile);

    // read lines from case - expected result of render
    bucket = expectedRenderResult;
    fgets(line, sizeof(line), testFile);
    bucket = stpcpy(bucket, line);

    snprintf(caseInfo, sizeof(caseInfo), "%s:%d", testFileName, file_line);

    // test next args
    rc = compareResults(actualNextStepResult, expectedNextStepResult, coreInputs, caseInfo, strcmp);

    // read empty line
    fgets(line, 255, testFile);

    file_line += 2 + 1;

    fclose(corePipe);
    if (rc != 0) {
      break;
    }
  }

  fclose(testFile);
  return rc;
}

void readER(FILE *fp, char initNextStepER[PIECE_COUNT][ARGS_SIZE], char initRenderER[PIECE_COUNT][FRAME_BUFFER_SIZE]) {
  char line[255];
  for (int i = 0; i < PIECE_COUNT; ++i) {
    fgets(line, 255, fp);
    stpcpy(initNextStepER[i], line);
    bucket = initRenderER[i];
    for (int j = 0; j < FRAME_LINES; ++j) {
      fgets(line, 255, fp);
      bucket = stpcpy(bucket, line);
    }

    //read empty line
    fgets(line, 255, fp);
  }
}

int init_tests(const char *core_path, const char *init_test_path) {
  char initNextStepER[PIECE_COUNT][ARGS_SIZE];
  char initRenderER[PIECE_COUNT][FRAME_BUFFER_SIZE];

  FILE *erFP = checkNull(fopen(init_test_path, "r"), init_test_path);
  readER(erFP, initNextStepER, initRenderER);
  return runInit(core_path, initNextStepER, initRenderER);
}

int main_tests(const char *core_path, const char *test_path) {
  if (isDirectory(test_path)) {
    return traverseAndExec(test_path, &runMain, core_path);
  } else {
    return runMain(test_path, core_path);
  }
}

int end_tests(const char *core_path, const char *end_test_path) {
  return traverseAndExec(end_test_path, &runEnd, core_path);
}

int main(int argc, char **argv) {
  if (argc < 5) {
    printf("%s\n", "provide arguments");
    return 1;
  }

  checkErrCode(init_tests(argv[1], argv[2]), "Init test failed");
  checkErrCode(main_tests(argv[1], argv[3]), "Main test failed");
  checkErrCode(end_tests(argv[1], argv[4]), "End test failed");


  printf("%s%s%s\n", GREEN, "SUCCESS", RESET);
  return EXIT_SUCCESS;
}
