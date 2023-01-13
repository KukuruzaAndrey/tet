#define BOARD_W 10
#define BOARD_H 20

#define FIG_COUNT 7
#define COLORS_COUNT 8

const int SCORES[] = {0, 10, 30, 60, 100};
enum MOVES {
  MOVE_DOWN = 0,
  MOVE_LEFT,
  MOVE_RIGHT,
  MOVE_ROTATE_CLOCKWISE,
  MOVE_ROTATE_COUNTER_CLOCKWISE,
  MOVE_DROP
};
const char *COLORS[] = {
    "", // for empty
    "\033[41m", // BackgroundRed
    "\033[42m", // BackgroundGreen
    "\033[43m", // BackgroundYellow
    "\033[44m", // BackgroundBlue
    "\033[45m", // BackgroundMagenta
    "\033[46m", // BackgroundCyan
    "\033[47m", // BackgroundWhite
};

struct coords {
  unsigned count;
  unsigned squares[4][2];
};

struct rotation {
  unsigned squares[4][2];
  unsigned ofx;
  unsigned ofy;
};

struct figure {
  unsigned count;
  struct rotation rotations[4];
};


const struct figure FIGURES[FIG_COUNT] = {
    { // I
        .count = 2,
        .rotations = {
            { .squares = {{0, 0}, {1, 0}, {2, 0}, {3, 0}}, .ofx = 0, .ofy = 2 },
            { .squares = {{0, 0}, {0, 1}, {0, 2}, {0, 3}}, .ofx = 2, .ofy = 0 },
        }
    },
    { // L
        .count = 4,
        .rotations = {
            { .squares = {{0, 0}, {1, 0}, {2, 0}, {0, 1}}, .ofx = 0, .ofy = 1 },
            { .squares = {{0, 0}, {1, 0}, {1, 1}, {1, 2}}, .ofx = 0, .ofy = 0 },
            { .squares = {{0, 1}, {1, 1}, {2, 1}, {2, 0}}, .ofx = 0, .ofy = 0 },
            { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 2}}, .ofx = 1, .ofy = 0 },
        }
    },
    { // J
        .count = 4,
        .rotations = {
            { .squares = {{0, 0}, {1, 0}, {2, 0}, {2, 1}}, .ofx = 0, .ofy = 1 },
            { .squares = {{1, 0}, {1, 1}, {1, 2}, {0, 2}}, .ofx = 0, .ofy = 0 },
            { .squares = {{0, 0}, {0, 1}, {1, 1}, {2, 1}}, .ofx = 0, .ofy = 0 },
            { .squares = {{0, 0}, {1, 0}, {0, 1}, {0, 2}}, .ofx = 1, .ofy = 0 },
        }
    },
    { // S
        .count = 2,
        .rotations = {
            { .squares = {{1, 0}, {2, 0}, {0, 1}, {1, 1}}, .ofx = 0, .ofy = 1 },
            { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 2}}, .ofx = 1, .ofy = 0 },
        }
    },
    { // Z
        .count = 2,
        .rotations = {
            { .squares = {{0, 0}, {1, 0}, {1, 1}, {2, 1}}, .ofx = 0, .ofy = 1 },
            { .squares = {{0, 1}, {1, 0}, {1, 1}, {0, 2}}, .ofx = 1, .ofy = 0 },
        }
    },
    { // O
        .count = 1,
        .rotations = {
            { .squares = {{0, 0}, {0, 1}, {1, 1}, {1, 0}}, .ofx = 0, .ofy = 0 },
        }
    },
    { // T
        .count = 4,
        .rotations = {
            { .squares = {{0, 0}, {1, 0}, {2, 0}, {1, 1}}, .ofx = 0, .ofy = 1 },
            { .squares = {{1, 0}, {0, 1}, {1, 1}, {1, 2}}, .ofx = 0, .ofy = 0 },
            { .squares = {{1, 0}, {0, 1}, {1, 1}, {2, 1}}, .ofx = 0, .ofy = 0 },
            { .squares = {{0, 0}, {0, 1}, {0, 2}, {1, 1}}, .ofx = 1, .ofy = 0 },
        }
    }
};

struct state {
  enum MOVES move;
  unsigned board[BOARD_H][BOARD_W];
  unsigned figIndex;
  unsigned rotateIndex;
  unsigned color;
  int offsetX;
  int offsetY;
  unsigned nextFigIndex;
  unsigned nextFigColor;
  unsigned score;
};

#define RESET "\x1B[m" // reset escape sequence
#define INVERSE "\x1B[7m" // inverse white and black part of letter square
#define CEIL "\u2582"
#define FLOOR INVERSE "\u2586" RESET
#define LEFT INVERSE "\u258a" RESET
#define RIGHT "\u258e"
#define SPACER "."

#define NEXT_P_BOARD_W 6
#define NEXT_P_BOARD_H 6
