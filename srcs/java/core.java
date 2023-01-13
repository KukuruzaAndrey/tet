import java.util.Arrays;

class core {
  public static void main(String[] args) {
    if (args.length == 10) {
      Game game = new Game(args);
      game.update();
      if (game.endGame()) {
        System.out.println("Game over!");
        return;
      }
      System.out.println(game.getNextState());
      System.out.println(game.render());
    } else if (args.length == 1 && args[0].equals("INIT_STATE")) {
      Game game = new Game();
      System.out.println(game.getNextState());
      System.out.println(game.render());
    } else {
      System.out.println("incorrect arguments");
    }
  }
}

class Random {
  public static int getRandomIntInclusive(int min, int max) {
    return (int)(Math.random() * (max - min + 1)) + min;
  }
}

class Rotation {
  public int[][] squares;
  public int ofx;
  public int ofy;

  public Rotation(int[][] squares, int ofx, int ofy) {
    this.squares = squares;
    this.ofx = ofx;
    this.ofy = ofy;
  }
}

class Figure {
  private static final Rotation[][] FIGURES = {
    { // I
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 } }, 0, 2),
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 0, 3 } }, 2, 0)
    },
    { // L
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 0, 1 } }, 0, 1),
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 1, 2 } }, 0, 0),
      new Rotation(new int[][] { { 0, 1 }, { 1, 1 }, { 2, 1 }, { 2, 0 } }, 0, 0),
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 2 } }, 1, 0)
    },
    { // J
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 2, 1 } }, 0, 1),
      new Rotation(new int[][] { { 1, 0 }, { 1, 1 }, { 1, 2 }, { 0, 2 } }, 0, 0),
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 0, 0),
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 0, 1 }, { 0, 2 } }, 1, 0)
    },
    { // S
      new Rotation(new int[][] { { 1, 0 }, { 2, 0 }, { 0, 1 }, { 1, 1 } }, 0, 1),
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 1, 0)
    },
    { // Z
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 1, 1 }, { 2, 1 } }, 0, 1),
      new Rotation(new int[][] { { 0, 1 }, { 1, 0 }, { 1, 1 }, { 0, 2 } }, 1, 0)
    },
    { // O
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 1, 1 }, { 1, 0 } }, 0, 0)
    },
    { // T
      new Rotation(new int[][] { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 1, 1 } }, 0, 1),
      new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 1, 2 } }, 0, 0),
      new Rotation(new int[][] { { 1, 0 }, { 0, 1 }, { 1, 1 }, { 2, 1 } }, 0, 0),
      new Rotation(new int[][] { { 0, 0 }, { 0, 1 }, { 0, 2 }, { 1, 1 } }, 1, 0)
    }
  };

  private final Rotation[] rotations;
  private final int figIndex;
  private int rotIndex;

  private static int getNextFigIndex() {
    return Random.getRandomIntInclusive(0, FIGURES.length - 1);
  }

  public Figure(int fi, int ri) {
    figIndex = fi;
    rotIndex = ri;
    rotations = FIGURES[figIndex];
  }

  public Figure() {
    figIndex = getNextFigIndex();
    rotIndex = 0;
    rotations = FIGURES[figIndex];
  }

  public int getIndex() {
    return figIndex;
  }

  public int getOfx() {
    return rotations[rotIndex].ofx;
  }

  public int getOfy() {
    return rotations[rotIndex].ofy;
  }

  public int[] getCoords(int i) {
    return rotations[rotIndex].squares[i];
  }

  public void nextRotIndex() {
    rotIndex = (rotIndex == rotations.length - 1) ? 0 : rotIndex + 1;
  }

  public void prevRotIndex() {
    rotIndex = (rotIndex == 0) ? rotations.length - 1 : rotIndex - 1;
  }

  @Override
  public String toString() {
    return figIndex + " " + rotIndex;
  }
}

class BoardFigure {
  private static final int COLORS_COUNT = 8;

  private final Figure figure;

  // need to be private
  public int color;
  public int offsetX;
  public int offsetY;

  private static int getNextFigColor() {
    return Random.getRandomIntInclusive(1, COLORS_COUNT - 1);
  }

  public BoardFigure() {
    figure = new Figure();
    color = getNextFigColor();
    offsetX = figure.getIndex() == 0 ? 3 : 4;
    offsetY = -1 * figure.getOfy();
  }

  public BoardFigure(int figIndex, int rotateIndex, int color, int offsetX, int offsetY) {
    figure = new Figure(figIndex, rotateIndex);
    this.color = color;
    this.offsetX = offsetX;
    this.offsetY = offsetY;
  }

  public BoardFigure(int figIndex, int color) {
    figure = new Figure(figIndex, 0);
    this.color = color;
    offsetX = figure.getIndex() == 0 ? 3 : 4;
    offsetY = -1 * figure.getOfy();
  }

  public BoardFigure forSmallBoard() {
    offsetX = figure.getIndex() == 5 ? 2 : 1;
    offsetY = figure.getIndex() == 5 ? 2 : 1;
    return this;
  }

  @Override
  public String toString() {
    return figure.toString() + " " + color + " " + offsetX +
      " " + offsetY;
  }

  public int[][] getCoords() {
    int [][] res = new int[4][2];
    for(int i = 0; i < 4; ++i) {
      res[i][0] = figure.getCoords(i)[0] + offsetX + figure.getOfx();
      res[i][1] = figure.getCoords(i)[1] + offsetY + figure.getOfy();
    }
    return res;
  }

  // crutch, need to be removed
  public void moveUp() {
    offsetY -= 1;
  }

  public void moveDown() {
    offsetY += 1;
  }
  public void moveLeft() {
    offsetX -= 1;
  }
  public void moveRight() {
    offsetX += 1;
  }

  public void rotateClockwise() {
    figure.nextRotIndex();
  }

  public void rotateCounterClockwise() {
    figure.prevRotIndex();
  }

  public int getFigIndex() {
    return figure.getIndex();
  }

}

abstract class RenderBoard {
  protected static int BOARD_W = 10;
  protected static int BOARD_H = 20;
  protected static final String[] COLORS = {
    "", // for empty
    "\033[41m", // BackgroundRed
    "\033[42m", // BackgroundGreen
    "\033[43m", // BackgroundYellow
    "\033[44m", // BackgroundBlue
    "\033[45m", // BackgroundMagenta
    "\033[46m", // BackgroundCyan
    "\033[47m" // BackgroundWhite
  };

  protected static int NEXT_P_BOARD_W = 6;
  protected static int NEXT_P_BOARD_H = 6;
  protected static String RESET = "\u001B[m";
  protected static String INVERSE = "\u001B[7m";
  protected static String CEIL = "\u2582";
  protected static String FLOOR = INVERSE + "\u2586" + RESET;
  protected static String LEFT = INVERSE + "\u258a" + RESET;
  protected static String RIGHT = "\u258e";
  protected static String SPACER = ".";
}

class SmallBoard extends RenderBoard {
  private int score;
  private BoardFigure bf;

  public SmallBoard() {
    score = 0;
    newFigure();
  }

  public SmallBoard(int nextFigIndex, int nextFigColor, int score) {
    this.score = score;
    bf = new BoardFigure(nextFigIndex, nextFigColor).forSmallBoard();
  }

  public void addScore(int score) {
    this.score += score;
  }

  public int getFigIndex() {
    return bf.getFigIndex();
  }

  public int getColor() {
    return bf.color;
  }

  public void newFigure() {
    this.bf = new BoardFigure().forSmallBoard();
  }

  public String renderLine(int y) {
    if (y > NEXT_P_BOARD_H + 2) return "";
    if (y == 0) return " " + String.format("%06d", score);
    if (y == 1) return " " + CEIL.repeat(NEXT_P_BOARD_W) + " ";
    if (y == NEXT_P_BOARD_H + 2) return " " + FLOOR.repeat(NEXT_P_BOARD_W) + " ";

    StringBuilder line = new StringBuilder();
    for (int x = 0; x < NEXT_P_BOARD_W; x++) {
      final int xx = x;
      final int yy = y;
      boolean isPiece = Arrays.stream(bf.getCoords()).anyMatch(pair -> {
          int xc = pair[0];
          int yc = pair[1];
          return xc == xx && yc == yy - 2;
        });
      if (isPiece) {
        line.append(COLORS[bf.color]).append(" ").append(RESET);
      } else {
        line.append(" ");
      }
    }

    return LEFT + line + RIGHT;
  }

  @Override
  public String toString() {
    return getFigIndex() + " " + getColor() + " " + score;
  }
}

class Board extends RenderBoard {
  private static final int[] SCORES = { 0, 10, 30, 60, 100 };

  private final int[][] board;
  private final SmallBoard sb;
  private BoardFigure bf;

  public Board() {
    board = new int[BOARD_H][BOARD_W];
    bf = new BoardFigure();
    sb = new SmallBoard();
  }

  public Board(String strBoard, BoardFigure pbf, SmallBoard psb) {
    board = new int[BOARD_H][BOARD_W];
    for (int i = 0; i < BOARD_H; i++) {
      for (int j = 0; j < BOARD_W; j++) {
        board[i][j] = Integer.parseInt(String.valueOf(strBoard.charAt(j + i * BOARD_W)));
      }
    }
    bf = pbf;
    sb = psb;
  }

  @Override
  public String toString() {
    StringBuilder strBoard = new StringBuilder();
    for (int i = 0; i < BOARD_H; i++) {
      for (int j = 0; j < BOARD_W; j++) {
        strBoard.append(board[i][j]);
      }
    }
    return String.format("%s %s %s", strBoard, bf, sb);
  }

  public boolean endGame() {
    return !canPlace();
  }

  private boolean isLegalCoords(int[][] coords)  {
    return Arrays.stream(coords).allMatch(pair -> {
        int x = pair[0];
        int y = pair[1];
        boolean isLegal = (y < BOARD_H && x >= 0 && x < BOARD_W);
        if (!isLegal) return false;
        // y < 0 - don't care about segments above top of the screen
        boolean isBoardCellFree = (y < 0 || board[y][x] == 0);
        return isBoardCellFree;
      });
  }

  private boolean canPlace() {
    int[][] coords = bf.getCoords();
    return isLegalCoords(coords);
  }

  private boolean tryMoveDown() {
    bf.moveDown();
    boolean can = canPlace();
    if (!can) bf.moveUp();
    return can;
  }

  private void addFigure() {
    Arrays.stream(bf.getCoords()).forEach(pair -> {
        int x = pair[0];
        int y = pair[1];
        // y < 0 - don't care about segments above top of the screen
        if (y < 0) return;
        board[y][x] = bf.color;
      });
  }

  private void clearFigure() {
    Arrays.stream(bf.getCoords()).forEach(pair -> {
        int x = pair[0];
        int y = pair[1];
        // y < 0 - don't care about segments above top of the screen
        if (y < 0) return;
        board[y][x] = 0;
      });
  }

  private int removeFullLines() {
    int fullLinesCount = 0;
    for (int y = 0; y < BOARD_H; y++) {
      boolean full = Arrays.stream(board[y]).allMatch(c -> c != 0);
      if (full) {
        System.arraycopy(board, 0, board, 1, y);
        board[0] = new int[BOARD_W];
        fullLinesCount++;
      }
    }

    return fullLinesCount;
  }

  private String renderLine(int y) {
    StringBuilder res = new StringBuilder();
    for(int x = 0; x < BOARD_W; x++) {
      if (board[y][x] != 0) {
        res.append(COLORS[board[y][x]]).append(" ").append(RESET);
      } else {
        res.append((x % 2 == 0) ? " " : SPACER);
      }
    }
    return res.toString();
  }

  public String render() {
    StringBuilder res = new StringBuilder();
    addFigure();

    res.append(" ").append(CEIL.repeat(BOARD_W)).append(" \n");

    for(int y = 0; y < BOARD_H; y++) {
      res.append(LEFT).append(renderLine(y)).append(RIGHT).append(sb.renderLine(y)).append("\n");
    }
    res.append(" ").append(FLOOR.repeat(BOARD_W)).append(" ");

    clearFigure();
    return res.toString();
  }

  private void processNewFigure() {
    addFigure();
    int fl = removeFullLines();
    sb.addScore(SCORES[fl]);
    bf = new BoardFigure(sb.getFigIndex(), sb.getColor());
    sb.newFigure();
  }

  public void moveDown() {
    boolean can = tryMoveDown();
    if (!can) {
      processNewFigure();
    }
  }

  public void moveLeft() {
    bf.moveLeft();
    if (!canPlace()) bf.moveRight();
  }

  public void moveRight() {
    bf.moveRight();
    if (!canPlace()) bf.moveLeft();
  }

  public void rotateClockwise() {
    bf.rotateClockwise();
    if (!canPlace()) bf.rotateCounterClockwise();
  }

  public void rotateCounterClockwise() {
    bf.rotateCounterClockwise();
    if (!canPlace()) bf.rotateClockwise();
  }

  public void drop() {
    while (tryMoveDown());
    processNewFigure();
  }

}

class Game {
  enum MOVES {
    DOWN,
    LEFT,
    RIGHT,
    ROTATE_CLOCKWISE,
    ROTATE_COUNTER_CLOCKWISE,
    DROP
  }

  private final MOVES move;
  private final Board board;

  public Game(String[] args) {
    move = MOVES.values()[Integer.parseInt(args[0])];
    BoardFigure bf = new BoardFigure(Integer.parseInt(args[2]), Integer.parseInt(args[3]), Integer.parseInt(args[4]), Integer.parseInt(args[5]), Integer.parseInt(args[6]));
    SmallBoard sb = new SmallBoard(Integer.parseInt(args[7]), Integer.parseInt(args[8]), Integer.parseInt(args[9]));
    board = new Board(args[1], bf, sb);
  }

  public Game() {
    move = MOVES.DOWN;
    board = new Board();
  }

  public void update() {
    switch (move) {
      case DOWN -> board.moveDown();
      case LEFT -> board.moveLeft();
      case RIGHT -> board.moveRight();
      case ROTATE_CLOCKWISE -> board.rotateClockwise();
      case ROTATE_COUNTER_CLOCKWISE -> board.rotateCounterClockwise();
      case DROP -> board.drop();
    }
  }

  public String getNextState() {
    return String.format("%d %s", move.ordinal(), board);
  }

  public String render() {
    return board.render();
  }

  public Boolean endGame() {
    return board.endGame();
  }

}
