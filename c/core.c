#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "core.h"

unsigned randomInt(void) {
  unsigned rand;
  FILE *fp;
  fp = fopen("/dev/urandom", "r");
  fread(&rand, sizeof(unsigned), 1, fp);
  fclose(fp);
  return rand;
}

unsigned randomIntInclusive(unsigned min, unsigned max) {
  return (randomInt() % (max - min + 1)) + min;
}

void initState(struct state *state) {
  unsigned figIndex = randomIntInclusive(0, FIG_COUNT - 1);
  state->move = tick;
  memset(state->board, 0, sizeof(state->board));
  state->figIndex = figIndex;
  state->rotateIndex = 0;
  state->color = randomIntInclusive(1, COLORS_COUNT - 1);
  state->offsetX = figIndex == 0 ? 3 : 4;
  state->offsetY = -1 * (int) figures[figIndex].rotations[0/*rotateIndex*/].ofy - 1;
  state->nextFigIndex = randomIntInclusive(0, FIG_COUNT - 1);
  state->nextFigColor = randomIntInclusive(1, COLORS_COUNT - 1);
  state->score = 0;
}

void printState(const struct state *state) {
  char strBoard[BOARD_H * BOARD_W + 1];
  for (unsigned y = 0; y < BOARD_H; ++y) {
    for (unsigned x = 0; x < BOARD_W; ++x) {
      strBoard[y * BOARD_W + x] = (char) ((unsigned) '0' + state->board[y][x]);
    }
  }
  strBoard[BOARD_H * BOARD_W] = 0;
  printf("%u %s %u %u %u %u %d %u %u %u\n",
         state->move, strBoard, state->figIndex, state->rotateIndex, state->color,
         state->offsetX, state->offsetY, state->nextFigIndex, state->nextFigColor, state->score);
}

void parseState(char **argv, struct state *dest) {
  dest->move = (enum move) (*argv[1] - '0');
  for (unsigned i = 0; i < BOARD_H * BOARD_W; ++i) {
    unsigned y = i / BOARD_W;
    unsigned x = i % BOARD_W;
    dest->board[y][x] = argv[2][i] - '0';
  }
  dest->figIndex = *argv[3] - '0';
  dest->rotateIndex = *argv[4] - '0';
  dest->color = *argv[5] - '0';
  dest->offsetX = *argv[6] - '0';
  dest->offsetY = *argv[7] - '0';
  dest->nextFigIndex = *argv[8] - '0';
  dest->nextFigColor = *argv[9] - '0';
  dest->score = *argv[10] - '0';
}

void calcFigCoords(unsigned squares[4][2], unsigned figIndex, unsigned rotateIndex, unsigned offsetX, int offsetY) {
  for (unsigned i = 0; i < 4; ++i) {
    // check overflow !
    squares[i][0] =
      figures[figIndex].rotations[rotateIndex].squares[i][0] + offsetX + figures[figIndex].rotations[rotateIndex].ofx;
    squares[i][1] =
      figures[figIndex].rotations[rotateIndex].squares[i][1] + offsetY + figures[figIndex].rotations[rotateIndex].ofy;
  }
  // filter negative!
}

void update(struct state *state) {
  unsigned oldCoords[4][2];
  calcFigCoords(oldCoords, state->figIndex, state->rotateIndex, state->offsetX, state->offsetY);

  unsigned newRotateIndex = state->rotateIndex;
  unsigned newOffsetX = state->offsetX;
  int newOffsetY = state->offsetY;

  // update piece position
  switch (state->move) {
    case tick:
    case down:
      newOffsetY += 1;
      break;
    case left:
      if (state->offsetX + figures[state->figIndex].rotations[state->rotateIndex].ofx <= 0)
        break;
      unsigned left_blocked = 0;
      for (unsigned i = 0; i < 4; ++i) {
        unsigned x = oldCoords[i][0];
        unsigned y = oldCoords[i][1];
        if (state->board[y][x - 1] != 0) {
          left_blocked = 1;
          break;
        }
      }
      if (!left_blocked) {
        newOffsetX -= 1;
      }
      break;
    case right:
      if (state->offsetX + figures[state->figIndex].rotations[state->rotateIndex].w +
          figures[state->figIndex].rotations[state->rotateIndex].ofx >= BOARD_W)
        break;
      unsigned right_blocked = 0;
      for (unsigned i = 0; i < 4; ++i) {
        unsigned x = oldCoords[i][0];
        unsigned y = oldCoords[i][1];
        if (state->board[y][x + 1] != 0) {
          right_blocked = 1;
          break;
        }
      }
      if (!right_blocked) {
        newOffsetX += 1;
      }
      break;
    case rotateClockwise: {
      unsigned possibleRotateIndex = (state->rotateIndex == figures[state->figIndex].count - 1) ? 0 :
                                     state->rotateIndex + 1;
      unsigned rotateFigCoords[4][2];
      calcFigCoords(rotateFigCoords, state->figIndex, possibleRotateIndex, state->offsetX, state->offsetY);
      unsigned rotate_clockwise_blocked = 0;
      for (unsigned i = 0; i < 4; ++i) {
        unsigned x = oldCoords[i][0];
        unsigned y = oldCoords[i][1];
        if (x < 0 || x >= BOARD_W || state->board[y][x] != 0) {
          rotate_clockwise_blocked = 1;
          break;
        }
      }
      if (!rotate_clockwise_blocked) {
        newRotateIndex = possibleRotateIndex;
      }
    }
      break;
    case rotateCounterClockwise: {
      unsigned possibleRotateIndex = (state->rotateIndex == 0) ? figures[state->figIndex].count - 1 :
                                     state->rotateIndex - 1;
      unsigned rotateFigCoords[4][2];
      calcFigCoords(rotateFigCoords, state->figIndex, possibleRotateIndex, state->offsetX, state->offsetY);
      unsigned rotate_counter_clockwise_blocked = 0;
      for (unsigned i = 0; i < 4; ++i) {
        unsigned x = oldCoords[i][0];
        unsigned y = oldCoords[i][1];
        if (x < 0 || x >= BOARD_W || state->board[y][x] != 0) {
          rotate_counter_clockwise_blocked = 1;
          break;
        }
      }
      if (!rotate_counter_clockwise_blocked) {
        newRotateIndex = possibleRotateIndex;
      }
    }
      break;
  }

  // calculate new coordinates
  unsigned newCoords[4][2];
  calcFigCoords(newCoords, state->figIndex, newRotateIndex, newOffsetX, newOffsetY);

  // check is new position is overlap or on floor
  unsigned overlap = 0;
  for (unsigned i = 0; i < 4; ++i) {
    unsigned x = oldCoords[i][0];
    unsigned y = oldCoords[i][1];
    if (y == BOARD_H || state->board[y][x] != 0) {
      overlap = 1;
      break;
    }
  }

  if (overlap) {
    // paint piece back
    for (unsigned i = 0; i < 4; ++i) {
      unsigned x = oldCoords[i][0];
      unsigned y = oldCoords[i][1];
      state->board[y][x] = state->color;
    }

    // remove full lines
    unsigned countFullLines = 0;
//    unsigned long fullLinesMask = 0;
    for (unsigned y = 0; y < BOARD_H; ++y) {
      unsigned full = 1;
      for (unsigned x = 0; x < BOARD_W; ++x) {
        if (state->board[y][x] == 0) {
          full = 0;
          break;
        }
      }
      if (full) {
        memmove(state->board[y], state->board[countFullLines], (y - countFullLines) * (sizeof state->board[y]));
        countFullLines++;
//        fullLinesMask |= (1 << y);
      }
    }

    if (countFullLines > 0) {
      // update score
      state->score += scores[countFullLines - 1];

      // add new empty lines
      memset(state->board[0], 0, countFullLines * (sizeof state->board[0]));
    }


  }


}

int main(int argc, char **argv) {
  struct state state;

  if (strcmp(argv[1], "0init") == 0) {
    initState(&state);
    printState(&state);
  } else if (argc == 11) {
    parseState(argv, &state);
    update(&state);
    printState(&state);
  } else {
    puts("incorrect arguments");
    exit(1);
  }

  return 0;
}

