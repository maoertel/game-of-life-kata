import GameOfLife.{Alive, Cell, Dead, Grid}
import cats.effect.IO
import cats.implicits._

import scala.util.{Failure, Success, Try}

object GameOfLife {

  type Grid = List[List[Cell]]

  sealed trait Cell

  case object Alive extends Cell

  case object Dead extends Cell

  def nextGen(grid: Grid): Grid =
    grid.zipWithIndex map { case (row: List[Cell], rowIndex: Int) =>
      row.zipWithIndex map { case (cell: Cell, colIndex: Int) =>
        getNextState(grid, cell, rowIndex, colIndex)
      }
    }

  def getNextState(grid: Grid, cell: Cell, rowIndex: Int, colIndex: Int): Cell = {
    val cellsAlive = evalNeighbors(grid, rowIndex, colIndex) count (_ == Alive)
    cell match {
      case Dead => if (cellsAlive == 3) Alive else Dead
      case Alive => if (cellsAlive < 2 || cellsAlive > 3) Dead else Alive
    }
  }

  def evalNeighbors(grid: Grid, row: Int, col: Int): List[Cell] =
    getNeighborCoordinates(grid: Grid, row: Int, col: Int) map { case (r: Int, c: Int) =>
      Try(grid(r)(c)) match {
        case Failure(_) => Dead
        case Success(cell) => cell
      }
    }

  def getNeighborCoordinates(grid: Grid, row: Int, col: Int) = List(
    (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
    (row,     col - 1),                 (row,     col + 1),
    (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
  )

}

object Printer {

  type PrintFX = List[IO[Unit]]

  def startGameOfLife(startGrid: Grid, generations: Int): IO[Unit] = {

    def printGrid(grid: Grid, effects: PrintFX): PrintFX = {

      @scala.annotation.tailrec
      def traverseOverGrid(restOfRows: Grid, effects: PrintFX): PrintFX =
        restOfRows match {
          case Nil => effects
          case row :: tail => traverseOverGrid(tail, traverseOverRow(row, effects) ++ (IO (println()) :: Nil))
        }

      @scala.annotation.tailrec
      def traverseOverRow(restOfCellsInRow: List[Cell], effects: PrintFX): PrintFX =
        restOfCellsInRow match {
          case Nil => effects
          case cell :: tail => traverseOverRow(tail, effects ++ (IO { print(if (cell == Alive) "X" else "_") } :: Nil))
        }

      traverseOverGrid(grid, effects)
    }

    @scala.annotation.tailrec
    def printAllGenerations(grid: Grid, restGenerations: Int, effects: PrintFX): PrintFX =
      if (restGenerations == 0) effects
      else printAllGenerations(GameOfLife.nextGen(grid), restGenerations - 1, effects ++ printGrid(grid, effects))

    printAllGenerations(startGrid, generations, Nil).sequence_
  }
}

object Main extends App {

  private val startGrid = List(
    List(Dead, Dead, Dead, Dead, Dead, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Dead, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Alive, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
  )

  private val game = Printer.startGameOfLife(startGrid, generations = 2)

  game.unsafeRunSync()
}
