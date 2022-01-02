const backColors = ['', // for empty
  '\u001b[41m', // BackgroundRed
  '\u001b[42m', // BackgroundGreen
  '\u001b[43m', // BackgroundYellow
  '\u001b[44m', // BackgroundBlue
  '\u001b[45m', // BackgroundMagenta
  '\u001b[46m', // BackgroundCyan
  '\u001b[47m', // BackgroundWhite
]
const Reset = '\u001b[m'
const boardW = 10
const boardH = 20
const scores = [10, 30, 60, 100]
const moves = {
  auto: 0, left: 1, right: 2, down: 3, rotateClockwise: 4, rotateCounterClockwise: 5,
}
const states = {
  start: 0, newPiece: 1, move: 2, beforeNewPiece: 3, end: 4
}
const figures = [
  [
    { squares: [[0, 0], [1, 0], [2, 0], [3, 0]], h: 1, w: 4, ofx: 0, ofy: 2 },
    { squares: [[0, 0], [0, 1], [0, 2], [0, 3]], h: 4, w: 1, ofx: 2, ofy: 0 },
  ], // I
  [
    { squares: [[0, 0], [1, 0], [2, 0], [0, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 0], [1, 0], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[0, 1], [1, 1], [2, 1], [2, 0]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // L
  [
    { squares: [[0, 0], [1, 0], [2, 0], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[1, 0], [1, 1], [1, 2], [0, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [1, 0], [0, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // Г
  [
    { squares: [[1, 0], [2, 0], [0, 1], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // S
  [
    { squares: [[0, 0], [1, 0], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[0, 1], [1, 0], [1, 1], [0, 2]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ], // Z
  [
    { squares: [[0, 0], [0, 1], [1, 1], [1, 0]], h: 2, w: 2, ofx: 0, ofy: 0 },
  ], // O
  [
    { squares: [[0, 0], [1, 0], [2, 0], [1, 1]], h: 2, w: 3, ofx: 0, ofy: 1 },
    { squares: [[1, 0], [0, 1], [1, 1], [1, 2]], h: 3, w: 2, ofx: 0, ofy: 0 },
    { squares: [[1, 0], [0, 1], [1, 1], [2, 1]], h: 2, w: 3, ofx: 0, ofy: 0 },
    { squares: [[0, 0], [0, 1], [0, 2], [1, 1]], h: 3, w: 2, ofx: 1, ofy: 0 },
  ]  // T
]

const getRandomIntInclusive = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min
}

const init = ({ board, figIndex, rotateIndex, color, offsetX, offsetY, move, nextFigIndex, nextFigColor, score }) => {
  figIndex = getRandomIntInclusive(0, figures.length - 1)
  nextFigIndex = getRandomIntInclusive(0, figures.length - 1)
  rotateIndex = 0 //getRandomIntInclusive(0, figures[figIndex].length - 1)
  offsetX = figIndex === 0 ? 3 : 4
  offsetY = -1 * figures[figIndex][rotateIndex].ofy
  color = getRandomIntInclusive(1, backColors.length - 1)
  nextFigColor = getRandomIntInclusive(1, backColors.length - 1)
  score = 0

  const coords = getFigCoords({
    figIndex, rotateIndex, offsetX, offsetY,
  })

  // add new piece to board
  for (const [x, y] of coords) {
    board[y][x] = color
  }

  return { board, figIndex, rotateIndex, color, offsetX, offsetY, move, nextFigIndex, nextFigColor, score }
}

const update = ({ board, figIndex, rotateIndex, color, offsetX, offsetY, move, nextFigIndex, nextFigColor, score }) => {
  // remove old piece
  const coords = getFigCoords({
    figIndex, rotateIndex, offsetX, offsetY,
  })
  for (const [x, y] of coords) {
    board[y][x] = 0
  }

  // update piece position
  switch (move) {
    case moves.auto:
    case moves.down:
      offsetY += 1
      break
    case moves.left:
      if ((offsetX + figures[figIndex][rotateIndex].ofx > 0) && coords.every(([x, y]) => board[y][x - 1] === 0)) {
        offsetX -= 1
      }
      break
    case moves.right:
      if ((offsetX + figures[figIndex][rotateIndex].w + figures[figIndex][rotateIndex].ofx < boardW) && coords.every(([x, y]) => board[y][x + 1] === 0)) {
        offsetX += 1
      }
      break
    case moves.rotateClockwise: {
      const newRotIndex = rotateIndex === figures[figIndex].length - 1 ? 0 : rotateIndex + 1
      const rotateFigCoords = getFigCoords({ figIndex, rotateIndex: newRotIndex, offsetX, offsetY })
      if (rotateFigCoords.every(([x, y]) => x >= 0 && x < boardW && board[y][x] === 0)) {
        rotateIndex = newRotIndex
      }
      break
    }
    case moves.rotateCounterClockwise: {
      const newRotIndex = rotateIndex === 0 ? figures[figIndex].length - 1 : rotateIndex - 1
      const rotateFigCoords = getFigCoords({ figIndex, rotateIndex: newRotIndex, offsetX, offsetY })
      if (rotateFigCoords.every(([x, y]) => x >= 0 && x < boardW && board[y][x] === 0)) {
        rotateIndex = newRotIndex
      }
      break
    }
  }

  // calculate new coordinates
  const newCoords = getFigCoords({
    figIndex, rotateIndex, offsetX, offsetY,
  })

  // check is new position is overlap or on floor
  if (newCoords.some((([x, y]) => (y === boardH) || board[y][x] !== 0))) {
    // paint piece back
    for (const [x, y] of coords) {
      board[y][x] = color
    }

    // remove full lines
    const boardWithoutFillLines = board.filter(line => line.some(c => c === 0))
    if (boardWithoutFillLines.length < boardH) {
      // update score
      score += scores[boardH - boardWithoutFillLines.length]

      // add new empty lines
      const newLines = []
      for (let i = 0; i < boardH - boardWithoutFillLines.length; i++) {
        newLines.push(Array(boardW).fill(0))
      }
      boardWithoutFillLines.unshift(...newLines)
      board = boardWithoutFillLines
    }

    // create new piece
    figIndex = nextFigIndex
    nextFigIndex = getRandomIntInclusive(0, figures.length - 1)
    rotateIndex = 0 //getRandomIntInclusive(0, figures[figIndex].length - 1)
    offsetX = figIndex === 0 ? 3 : 4
    offsetY = -1 * figures[figIndex][rotateIndex].ofy
    color = nextFigColor
    nextFigColor = getRandomIntInclusive(1, backColors.length - 1)

    const newCoords = getFigCoords({
      figIndex, rotateIndex, offsetX, offsetY,
    })

    // check end of game
    if (newCoords.some((([x, y]) => board[y][x] !== 0))) {
      console.log('The End')
      process.exit()
      return
    }

    // add new piece to board
    for (const [x, y] of newCoords) {
      board[y][x] = color
    }
  } else {
    // add updated piece to board
    for (const [x, y] of newCoords) {
      board[y][x] = color
    }
  }

  return { board, figIndex, rotateIndex, color, offsetX, offsetY, move, nextFigIndex, nextFigColor, score }
}
const getFigCoords = ({ figIndex, rotateIndex, offsetX, offsetY, }) =>
  figures[figIndex][rotateIndex].squares
    .map(([x, y]) => [x + offsetX + figures[figIndex][rotateIndex].ofx, y + offsetY + figures[figIndex][rotateIndex].ofy])
    .filter(([x, y]) => y >= 0)

// const border = '\u001B[7m \u001B[m'
const Inverse = '\u001B[7m'
const ceil = '\u2582'
const floor = Inverse + '\u2586' + Reset
const left = Inverse + '\u258a' + Reset
const right = '\u258e'

const renderNextPiece = (figIndex, color) => {
  const w = 6
  const h = 6
  const offsetX = figIndex === 5 ? 2 : 1
  const offsetY = figIndex === 5 ? 2 : 1
  const coords = getFigCoords({
    figIndex, rotateIndex: 0, offsetX, offsetY
  })

  const resArr = []
  let res = ''
  res += ' '
  for (let x = 0; x < w; x++) {
    res += ceil
  }
  res += ' '
  resArr.push(res)
  res = ''

  for (let y = 0; y < h; y++) {
    res += left
    for (let x = 0; x < w; x++) {
      if (coords.some(([xc, yc]) => xc === x && yc === y)) {
        res += backColors[color] + ' ' + Reset
      } else {
        res += ' '
      }
    }
    res += right
    resArr.push(res)
    res = ''
  }
  res += ' '
  for (let x = 0; x < w; x++) {
    res += floor
  }
  res += ' '
  resArr.push(res)

  return resArr
}

const render = ({ board, nextFigIndex, nextFigColor, score }) => {
  const nP = renderNextPiece(nextFigIndex, nextFigColor)
  let res = ''
  res += ' '
  for (let x = 0; x < boardW; x++) {
    res += ceil
  }
  res += ' '
  res += '\n'

  for (let y = 0; y < boardH; y++) {
    res += left
    for (let x = 0; x < boardW; x++) {
      if (board[y][x] !== 0) {
        res += backColors[board[y][x]] + ' ' + Reset
      } else {
        res += ' '
      }
    }
    res += right

    if (y === 0) {
      res += ' ' + String(score).padStart(6, '0')
    }

    if (y > 0 && y - 1 < nP.length) {
      res += nP[y - 1]
    }

    res += '\n'
  }
  res += ' '
  for (let x = 0; x < boardW; x++) {
    res += floor
  }
  res += ' '
  // res += '\n'

  return res
}

const newBoard = []
for (let y = 0; y < boardH; y++) {
  const row = Array(boardW).fill(0)
  newBoard.push(row)
}

let state = { board: newBoard, move: moves.auto, score: 0 }
state = init(state)
console.log(render(state))
const step = () => {
  state = update(state)
  console.log(render(state))
}
const int = setInterval(() => {
  if (state.state !== states.end) {
    state.move = moves.auto
    step(state)
  } else {
    clearInterval(int)
    console.log('the end')
    process.exit()
  }
}, 400)
const stdin = process.stdin
stdin.setRawMode(true)
// console.log(stdin.isTTY)
stdin.setEncoding('utf8')

stdin.on('data', chunk => {
  switch (chunk) {
    case '\u001b[A':
      // console.log('Up')
      break
    case '\u001b[B':
      state.move = moves.down
      step(state)
      break
    case '\u001b[C':
      state.move = moves.right
      step(state)
      break
    case '\u001b[D':
      state.move = moves.left
      step(state)
      break
    case 'z':
      state.move = moves.rotateClockwise
      step(state)
      break
    case 'x':
      state.move = moves.rotateCounterClockwise
      step(state)
      break
    case '\u0003':
      console.log('Exit')
      process.exit()
      break
    case 'something_else':
      // Perform what something_else does
      break
  }
})