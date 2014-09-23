/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package reversi

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport

sealed abstract class OptPlayer

sealed abstract class Player extends OptPlayer {
  val opponent: Player
}

case object NoPlayer extends OptPlayer

case object White extends Player {
  val opponent = Black
}

case object Black extends Player {
  val opponent = White
}

@JSExport("Reversi")
class Reversi(jQuery: JQueryStatic, playground: JQuery) {

  // The Model -----------------------------------------------------------------

  val BoardSize = 8 // size of a Reversi board

  def inBounds(index: Int): Boolean = index >= 0 && index < BoardSize
  def inBounds(x: Int, y: Int): Boolean = inBounds(x) && inBounds(y)

  class Square(val x: Int, val y: Int) {
    private var _owner: OptPlayer = NoPlayer

    var onOwnerChange: (OptPlayer, OptPlayer) => Unit = (oldP, newP) => ()

    def owner = _owner
    def owner_=(value: OptPlayer) {
      val previous = _owner
      if (value != previous) {
        _owner = value
        onOwnerChange(previous, value)
      }
    }

    override def toString() = "Square("+x+", "+y+", "+owner+")"
  }

  val board = Array.tabulate[Square](BoardSize, BoardSize)(new Square(_, _))
  val allSquares = board.flatten
  var currentPlayer: Player = White // Irrelevant, set again in startGame()

  // The GUI -------------------------------------------------------------------

  val resetButton = createResetButton()
  val passButton = createPassButton()
  val status = createStatus()
  buildUI()

  def createResetButton() = {
    jQuery("<input>", js.Dynamic.literal(
        `type` = "button", value = "Reset"
    )).click(reset _)
  }

  def createPassButton() = {
    jQuery("<input>", js.Dynamic.literal(
        `type` = "button", value = "Pass"
    )).click(pass _)
  }

  def createStatus() = {
    jQuery("<span>")
  }

  def buildUI() {
    // Some dimensions
    val SquareSizePx = 48
    val HalfSquareSizePx = SquareSizePx/2
    val PawnRadiusPx = HalfSquareSizePx-4
    val BoardSizePx = BoardSize*SquareSizePx + 3

    // Creat the board canvas
    val boardCanvas = jQuery(
        "<canvas width='"+BoardSizePx+"' height='"+BoardSizePx+"'></canvas>")
    val domCanvas = boardCanvas.get(0).asInstanceOf[HTMLCanvasElement]
    val context = domCanvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]

    playground.append(jQuery("<div>").append(boardCanvas))

    /** Draw the specified square on the board canvas */
    def drawSquare(square: Square) {
      val x = square.x * SquareSizePx
      val y = square.y * SquareSizePx

      // Background
      context.fillStyle = "green"
      context.fillRect(x, y, SquareSizePx, SquareSizePx)

      // Border
      context.fillStyle = "black"
      context.lineWidth = 3
      context.strokeRect(x, y, SquareSizePx, SquareSizePx)

      // Pawn
      if (square.owner != NoPlayer) {
        context.fillStyle = if (square.owner == White) "white" else "black"
        context.beginPath()
        context.arc(x+HalfSquareSizePx, y+HalfSquareSizePx, PawnRadiusPx,
            0, 2*Math.PI, true)
        context.fill()
      }
    }

    // Draw squares now, and everytime they change ownership
    for (square <- allSquares) {
      drawSquare(square)
      square.onOwnerChange = { (prevOwner, newOwner) =>
        drawSquare(square)
      }
    }

    // Configure clicks on the board
    boardCanvas.click { (event: JQueryEvent) =>
      val offsetX = event.pageX - boardCanvas.offset().left
      val offsetY = event.pageY - boardCanvas.offset().top
      val x = offsetX.toInt / SquareSizePx
      val y = offsetY.toInt / SquareSizePx

      if (inBounds(x, y))
        clickSquare(board(x)(y))
    }

    // Build the status bar
    val statusBar = jQuery("<p>")
    statusBar.append(resetButton)
    statusBar.append(status)
    statusBar.append(passButton)
    playground.append(statusBar)
  }

  // The Game ------------------------------------------------------------------

  def reset() {
    startGame()
  }

  @JSExport
  def startGame() {
    // Set up the board
    allSquares foreach (_.owner = NoPlayer)
    board(3)(3).owner = White
    board(3)(4).owner = Black
    board(4)(3).owner = Black
    board(4)(4).owner = White

    // White begins
    currentPlayer = White

    // Let's go!
    startTurn()
  }

  def startTurn() {
    val (scoreWhite, scoreBlack) = computeScore()
    status.text(currentPlayer+"'s turn -- White: "+scoreWhite+
        " -- Black: "+scoreBlack)

    passButton.prop("disabled", true)

    if (!existsValidMove()) {
      // Test if the other player can do something
      currentPlayer = currentPlayer.opponent
      val opponentCanDoSomething = existsValidMove()
      currentPlayer = currentPlayer.opponent

      if (opponentCanDoSomething) {
        passButton.prop("disabled", false)
      } else {
        // End of game
        val winnerText =
          if (scoreWhite > scoreBlack) "White won!"
          else if (scoreBlack > scoreWhite) "Black won!"
          else "Draw"
        status.text("Game finished -- White: "+scoreWhite+
            " -- Black: "+scoreBlack+" -- "+winnerText)
      }
    }
  }

  def clickSquare(square: Square) {
    val toFlip = computeFlips(square)
    if (!toFlip.isEmpty) {
      (square :: toFlip) foreach (_.owner = currentPlayer)
      nextTurn()
    }
  }

  def pass() {
    assert(!existsValidMove())
    nextTurn()
  }

  def existsValidMove(): Boolean = {
    allSquares.exists(isValidMove)
  }

  def isValidMove(square: Square): Boolean = {
    !computeFlips(square).isEmpty
  }

  def computeFlips(square: Square): List[Square] = {
    if (square.owner != NoPlayer) Nil
    else {
      for {
        i <- (-1 to 1).toList
        j <- -1 to 1
        if i != 0 || j != 0
        flip <- computeFlipsInDirection(square.x, square.y, i, j)
      } yield flip
    }
  }

  def computeFlipsInDirection(x: Int, y: Int,
      dirx: Int, diry: Int): List[Square] = {

    val allInDir = allSquaresInDirection(x, y, dirx, diry)
    val (toFlip, remaining) =
      allInDir.span(_.owner == currentPlayer.opponent)

    val success = remaining.headOption.exists(_.owner == currentPlayer)
    if (success) toFlip
    else Nil
  }

  def allSquaresInDirection(fromx: Int, fromy: Int,
      dirx: Int, diry: Int): List[Square] = {
    val nextx = fromx + dirx
    val nexty = fromy + diry
    if (inBounds(nextx, nexty))
      board(nextx)(nexty) :: allSquaresInDirection(nextx, nexty, dirx, diry)
    else
      Nil
  }

  def computeScore(): (Int, Int) = {
    allSquares.foldLeft((0, 0)) { case ((white, black), square) =>
      square.owner match {
        case White => (white+1, black)
        case Black => (white, black+1)
        case NoPlayer => (white, black)
      }
    }
  }

  def nextTurn() {
    currentPlayer = currentPlayer.opponent
    startTurn()
  }
}
