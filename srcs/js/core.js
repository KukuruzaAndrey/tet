const BOARD_W = 10
const BOARD_H = 20
const SCORES = [0, 10, 30, 60, 100]
const MOVES = {
  DOWN: 0,
  LEFT: 1,
  RIGHT: 2,
  ROTATE_CLOCKWISE: 3,
  ROTATE_COUNTER_CLOCKWISE: 4,
  DROP: 5,
}

const COLORS = [
  '', // for empty
  '\033[41m', // BackgroundRed
  '\033[42m', // BackgroundGreen
  '\033[43m', // BackgroundYellow
  '\033[44m', // BackgroundBlue
  '\033[45m', // BackgroundMagenta
  '\033[46m', // BackgroundCyan
  '\033[47m', // BackgroundWhite
]

const FIGURES = [
  [ // I
    { squares: [[0, 0], [1, 0], [2, 0], [3, 0]], ofx: 0, ofy: 2 },
    { squares: [[0, 0], [0, 1], [0, 2], [0, 3]], ofx: 2, ofy: 0 },
  ],
  [ // L
    { squares: [[0, 0], [1, 0], [2, 0], [0, 1]], ofx: 0, ofy: 1 },
    { squares: [[0, 0], [1, 0], [1, 1], [1, 2]], ofx: 0, ofy: 0 },
    { squares: [[0, 1], [1, 1], [2, 1], [2, 0]], ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 2]], ofx: 1, ofy: 0 },
  ],
  [ // J
    { squares: [[0, 0], [1, 0], [2, 0], [2, 1]], ofx: 0, ofy: 1 },
    { squares: [[1, 0], [1, 1], [1, 2], [0, 2]], ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [1, 1], [2, 1]], ofx: 0, ofy: 0 },
    { squares: [[0, 0], [1, 0], [0, 1], [0, 2]], ofx: 1, ofy: 0 },
  ],
  [ // S
    { squares: [[1, 0], [2, 0], [0, 1], [1, 1]], ofx: 0, ofy: 1 },
    { squares: [[0, 0], [0, 1], [1, 1], [1, 2]], ofx: 1, ofy: 0 },
  ],
  [ // Z
    { squares: [[0, 0], [1, 0], [1, 1], [2, 1]], ofx: 0, ofy: 1 },
    { squares: [[0, 1], [1, 0], [1, 1], [0, 2]], ofx: 1, ofy: 0 },
  ],
  [ // O
    { squares: [[0, 0], [0, 1], [1, 1], [1, 0]], ofx: 0, ofy: 0 },
  ],
  [ // T
    { squares: [[0, 0], [1, 0], [2, 0], [1, 1]], ofx: 0, ofy: 1 },
    { squares: [[1, 0], [0, 1], [1, 1], [1, 2]], ofx: 0, ofy: 0 },
    { squares: [[1, 0], [0, 1], [1, 1], [2, 1]], ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 1]], ofx: 1, ofy: 0 },
  ]
]

const getRandomIntInclusive = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min
}

const createBoard = (BOARD_W, BOARD_H) => {
  const board = []
  for (let y = 0; y < BOARD_H; y++) {
    const row = Array(BOARD_W).fill(0)
    board.push(row)
  }
  return board
}

const init = () => {
  const move = MOVES.DOWN
  const board = createBoard(BOARD_W, BOARD_H)
  const figIndex = getRandomIntInclusive(0, FIGURES.length - 1)
  const rotateIndex = 0
  const color = getRandomIntInclusive(1, COLORS.length - 1)
  const offsetX = figIndex === 0 ? 3 : 4
  const offsetY = -1 * FIGURES[figIndex][rotateIndex].ofy // ???
  const nextFigIndex = getRandomIntInclusive(0, FIGURES.length - 1)
  const nextFigColor = getRandomIntInclusive(1, COLORS.length - 1)
  const score = 0

  return { move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }
}

const getFigCoords = (figIndex, rotateIndex, offsetX, offsetY) => {
  let coords = FIGURES[figIndex][rotateIndex].squares
      .map(([x, y]) => [x + offsetX + FIGURES[figIndex][rotateIndex].ofx, y + offsetY + FIGURES[figIndex][rotateIndex].ofy])
  // first we need check is fig position is legal
  const illegal = coords.some(([x, y]) => (y >= BOARD_H || x < 0 || x >= BOARD_W))
  if (illegal)
    return null
  // and only then filter segments above top
  coords = coords.filter(([_, y]) => y >= 0)
  return coords
}

const boardCellsFree = (coords, board) => coords.every(([x, y]) => board[y][x] === 0)

const removeFullLines = state => {
  const boardWithoutFillLines = state.board.filter(line => line.some(c => c === 0))
  if (boardWithoutFillLines.length < BOARD_H) {
    // update score
    state.score += SCORES[BOARD_H - boardWithoutFillLines.length]

    // add new empty lines
    const newLines = []
    for (let i = 0; i < BOARD_H - boardWithoutFillLines.length; i++) {
      newLines.push(Array(BOARD_W).fill(0))
    }
    boardWithoutFillLines.unshift(...newLines)
    state.board = boardWithoutFillLines
  }
}

const createNewFig = state => {
  // replace known things
  state.figIndex = state.nextFigIndex
  state.color = state.nextFigColor

  state.offsetX = state.figIndex === 0 ? 3 : 4
  state.offsetY = -1 * FIGURES[state.figIndex][0].ofy
  state.rotateIndex = 0

  // calculate new
  state.nextFigIndex = getRandomIntInclusive(0, FIGURES.length - 1)
  state.nextFigColor = getRandomIntInclusive(1, COLORS.length - 1)
}

const canPlace = (board, figIndex, rotateIndex, offsetX, offsetY) => {
  const coords = getFigCoords(figIndex, rotateIndex, offsetX, offsetY)
  return coords && boardCellsFree(coords, board)
}

const checkEndGame = ({board, figIndex, rotateIndex, offsetX, offsetY}) => {
  return !canPlace(board, figIndex, rotateIndex, offsetX, offsetY)
}

const needNewFigure = ({board, figIndex, rotateIndex, offsetX, offsetY}) => {
  return !canPlace(board, figIndex, rotateIndex, offsetX, offsetY + 1)
}

const canMoveLeft = ({board, figIndex, rotateIndex, offsetX, offsetY}) => {
  return canPlace(board, figIndex, rotateIndex, offsetX - 1, offsetY)
}
const canMoveRight = ({board, figIndex, rotateIndex, offsetX, offsetY}) => {
  return canPlace(board, figIndex, rotateIndex, offsetX + 1, offsetY)
}


const drop = state => {
  while (!needNewFigure(state)) {
    state.offsetY += 1
  }
}

const addPieceToBoard = ({board, figIndex, color, rotateIndex, offsetX, offsetY}) => {
  const coords = getFigCoords(figIndex, rotateIndex, offsetX, offsetY)
  for (const [x, y] of coords) {
    board[y][x] = color
  }
}

const clearPieceAtBoard = ({board, figIndex, rotateIndex, offsetX, offsetY}) => {
  const coords = getFigCoords(figIndex, rotateIndex, offsetX, offsetY)
  for (const [x, y] of coords) {
    board[y][x] = 0
  }
}

const processNewFigure = state => {
  addPieceToBoard(state)
  removeFullLines(state)
  createNewFig(state)
}

const update = (state) => {
  ({ move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score } = state)
  switch (move) {
  case MOVES.DOWN:
    if (needNewFigure(state)) {
      processNewFigure(state)
      return state
    }
    state.offsetY += 1
    return state
  case MOVES.LEFT:
    if (canMoveLeft(state)) {
      state.offsetX -= 1
      return state
    }
    break
  case MOVES.RIGHT:
    if (canMoveRight(state)) {
      state.offsetX += 1
      return state
    }
    break
  case MOVES.ROTATE_CLOCKWISE: {
    const newRotIndex = rotateIndex === FIGURES[figIndex].length - 1 ? 0 : rotateIndex + 1
    if (canPlace(board, figIndex, newRotIndex, offsetX, offsetY)) {
      state.rotateIndex = newRotIndex
      return state
    }
    break
  }
  case MOVES.ROTATE_COUNTER_CLOCKWISE: {
    const newRotIndex = rotateIndex === 0 ? FIGURES[figIndex].length - 1 : rotateIndex - 1
    if (canPlace(board, figIndex, newRotIndex, offsetX, offsetY)) {
      state.rotateIndex = newRotIndex
      return state
    }
    break
  }
  case MOVES.DROP:
    drop(state)
    processNewFigure(state)
    return state
  }

  return state
}


const RESET = '\x1B[m' // reset escape sequence
const INVERSE = '\x1B[7m' // inverse white and black part of letter square
const CEIL = '\u2582'
const FLOOR = INVERSE + '\u2586' + RESET
const LEFT = INVERSE + '\u258a' + RESET
const RIGHT = '\u258e'
const SPACER = '.'
const NEXT_P_BOARD_W = 6
const NEXT_P_BOARD_H = 6

const render = ({ move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }) => {

  // add piece to board for simplifying render
  addPieceToBoard({board, figIndex, rotateIndex, color, offsetX, offsetY})

  const renderLine = y => {
    let line = ''
    for (let x = 0; x < BOARD_W; x++) {
      if (board[y][x] !== 0) {
        line += COLORS[board[y][x]] + ' ' + RESET
      } else {
        line += (x % 2 === 0) ? ' ' : SPACER
      }
    }
    return line
  }
  const coords = getFigCoords(nextFigIndex, 0, nextFigIndex === 5 ? 2 : 1, nextFigIndex === 5 ? 2 : 1)
  const renderNextPieceLine = y => {
    if (y > NEXT_P_BOARD_H + 2) return ''
    if (y === 0) return ' ' + String(score).padStart(6, '0')
    if (y === 1) return ` ${CEIL.repeat(NEXT_P_BOARD_W)} `
    if (y === NEXT_P_BOARD_H + 2) return ` ${FLOOR.repeat(NEXT_P_BOARD_W)} `
    let line = ''
    for (let x = 0; x < NEXT_P_BOARD_W; x++) {
      if (coords.some(([xc, yc]) => xc === x && yc === y - 2)) {
        line += COLORS[nextFigColor] + ' ' + RESET
      } else {
        line += ' '
      }
    }
    return LEFT + line + RIGHT
  }


  let res = ` ${CEIL.repeat(BOARD_W)} \n`
  for (let y = 0; y < BOARD_H; y++) {
    res += LEFT + renderLine(y) + RIGHT + renderNextPieceLine(y) + '\n'
  }
  res += ` ${FLOOR.repeat(BOARD_W)} `

  clearPieceAtBoard({board, figIndex, rotateIndex, offsetX, offsetY})

  return res
}

const stateToStr =
      ({
        move,
        board,
        figIndex,
        rotateIndex,
        color,
        offsetX,
        offsetY,
        nextFigIndex,
        nextFigColor,
        score
      }) => {
        const strBoard = board.map(row => row.join('')).join('')
        return `${move} ${strBoard} ${figIndex} ${rotateIndex} ${color} ${offsetX} ${offsetY} ${nextFigIndex} ${nextFigColor} ${score}`
      }

const parseState = () => {
  const move = parseInt(process.argv[2])
  const board = []
  for (let i = 0; i < process.argv[3].length; i += BOARD_W) {
    board.push(process.argv[3].slice(i, i + BOARD_W).split('').map(c => parseInt(c)))
  }
  const figIndex = parseInt(process.argv[4])
  const rotateIndex = parseInt(process.argv[5])
  const color = parseInt(process.argv[6])
  const offsetX = parseInt(process.argv[7])
  const offsetY = parseInt(process.argv[8])
  const nextFigIndex = parseInt(process.argv[9])
  const nextFigColor = parseInt(process.argv[10])
  const score = parseInt(process.argv[11])
  return { move, board, figIndex, rotateIndex, color, offsetX, offsetY, nextFigIndex, nextFigColor, score }
}

if (process.argv.length === 12) {
  const state = update(parseState())
  if (checkEndGame(state)) {
      console.log('Game over!')
      return
  }
  console.log(stateToStr(state))
  console.log(render(state))
} else if (process.argv.length === 3 && process.argv[2] === 'INIT_STATE') {
  const state = init()
  console.log(stateToStr(state))
  console.log(render(state))
} else {
  console.log(process.argv)
  throw Error('incorrect arguments')
}
