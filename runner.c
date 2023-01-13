#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include <string.h>
#include <unistd.h>
#include <termios.h>
#include <bits/types/struct_timeval.h>
#include <sys/select.h>
#include "utils.h"

#define FRAME_TIME_MS (400)
#define FRAME_TIME_NS (FRAME_TIME_MS * 1000 * 1000)
#define CTRL_KEY(k) ((k) & 0x1f)
#define INPUT_SIZE 500

#define INIT_STATE "INIT_STATE\n"

struct termios original;

static uint32_t get_nanos(void) {
  struct timespec ts;
  timespec_get(&ts, TIME_UTC);
  return (long) ts.tv_sec * 1000000000L + ts.tv_nsec;
}

int8_t kbhit() {
  struct timeval tv = {0L, 0L};
  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(0, &fds);
  return select(1, &fds, NULL, NULL, &tv) > 0;
}

void disableRawMode() {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &original);
}

void enableRawMode() {
  tcgetattr(STDIN_FILENO, &original);
  atexit(disableRawMode);

  struct termios raw = original;
  raw.c_iflag &= ~(ICRNL | IXON);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

FILE *logs;

char processKeypress() {
  char c, n;
  read(STDIN_FILENO, &c, sizeof(c));
  switch (c) {
    case CTRL_KEY('q'):
      fputs("=== END ===\n", logs);
      fclose(logs);
      exit(0);
    case '\x1b':
      read(STDIN_FILENO, &n, 1);
      if (n == '[') {
        read(STDIN_FILENO, &n, 1);
        // down(tick): 0, left: 1, right: 2, rotateClockwise: 3, rotateCounterClockwise: 4, drop: 5
        switch (n) {
          case 'A':
            return '5';
          case 'B':
            return '0';
          case 'C':
            return '2';
          case 'D':
            return '1';
        }
      }
      break;
    case 'z':
      return '3';
    case 'x':
      return '4';
    default:
      return 0;
  }
  return 0;
}

void eval(const char *corePath, char *coreInputs) {
  char coreArgs[1035];
  char resLineBuff[1035];

  // Concatenate path-to-core and inputs aka tetris state
  snprintf(coreArgs, sizeof(coreArgs), "%s %s", corePath, coreInputs);
  // printf("runner: args: %s\n", coreArgs);
  // Open pipe with core
  FILE *core = checkNull(popen(coreArgs, "r"), coreArgs);
  fprintf(logs, "%s", coreInputs);

  unsigned line = 0;
  while (fgets(resLineBuff, sizeof(resLineBuff), core) != NULL) {
    if (line == 0) {
      strncpy(coreInputs, resLineBuff, INPUT_SIZE);
      if (strcmp(resLineBuff, "Game over!\n") == 0) {
        fputs("Game over!\n", logs);
        printf("%s\n", "Game over!");
        fclose(logs);
        exit(0);
      }
      fputs(resLineBuff, logs);
      line = 1;
    } else {
      printf("%s", resLineBuff);
      fputs(resLineBuff, logs);
    }
  }
  fputs("\n", logs);
  fclose(core);
}

int validateInputs(int argc, char **argv) {
  if (argc == 2) {
  } else if (argc == 12) {
    if (strlen(argv[3]) != 200) {
      printf("board arg must have 200 chars, but have %ld\n", strlen(argv[3]));
      return 1;
    }
  } else {
    printf("%s\n", "incorrect number of arguments");
    return 1;
  }
  return 0;
}

int main(int argc, char **argv) {
  if (validateInputs(argc, argv) > 0) {
    exit(1);
  }

  char *corePath = argv[1];
  char coreInputs[INPUT_SIZE] = INIT_STATE;
  if (argc == 12) {
    sprintf(coreInputs, "%s %s %s %s %s %s %s %s %s %s\n", argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
            argv[8], argv[9], argv[10], argv[11]);
  }

  logs = checkNull(fopen("logs.txt", "a+"), "logs.txt");


  enableRawMode();
  uint32_t nanos;
  uint32_t prev_nanos;
  nanos = get_nanos();
  prev_nanos = nanos;
  char keyPressed = 0;
  // if we dont provide init state, first arg is INIT_STATE
  if (!strcmp(coreInputs, INIT_STATE)) {
    eval(corePath, coreInputs);
  }
  while (1) {
    nanos = get_nanos();
    if (kbhit()) {
      keyPressed = processKeypress();
    }
    if (nanos - prev_nanos > FRAME_TIME_NS) {
      prev_nanos = nanos;
      keyPressed = '0';
    }
    if (keyPressed) {
      coreInputs[0] = keyPressed;
      eval(corePath, coreInputs);
      keyPressed = 0;
    }
  }
  return EXIT_SUCCESS;
}
