import GameOfLife.{Alive, Cell, Dead, Grid}

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

  def startGameOfLife(startGrid: Grid, generations: Int): Unit = {

    def printGrid(grid: Grid): Unit = {

      @scala.annotation.tailrec
      def traverseOverGrid(restOfRows: List[List[Cell]]): Unit =
        restOfRows match {
          case Nil => ()
          case row :: tail =>
            traverseOverRow(row)
            println()
            traverseOverGrid(tail)
        }

      @scala.annotation.tailrec
      def traverseOverRow(restOfCellsInRow: List[Cell]): Unit =
        restOfCellsInRow match {
          case Nil => ()
          case cell :: tail =>
            print(if (cell == Alive) "X" else "_")
            traverseOverRow(tail)
        }

      traverseOverGrid(grid)
    }

    @scala.annotation.tailrec
    def printAllGenerations(grid: Grid, restGenerations: Int): Unit =
      if (restGenerations == 0) ()
      else {
        printGrid(grid: Grid)
        printAllGenerations(GameOfLife.nextGen(grid), restGenerations - 1)
      }

    printAllGenerations(startGrid, generations)
  }
}

object Main extends App {

  val startGrid = List(
    List(Dead, Dead, Dead, Dead, Dead, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Dead, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Alive, Alive, Dead, Dead),
    List(Dead, Dead, Dead, Dead, Dead, Dead, Dead, Dead),
  )

  Printer.startGameOfLife(startGrid, generations = 100)
}
