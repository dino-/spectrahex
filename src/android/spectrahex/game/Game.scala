package android.spectrahex.game

import java.util.Properties
import scala.util.Random

import android.spectrahex.game.color._
import android.spectrahex.game.color.Color._
import android.spectrahex.Util._


case class Pos (x: Int, y: Int)

object Pos {

   def fromProperty (s: String): Pos = {
      val regex = "Pos\\((\\d+),(\\d+)\\)".r

      s match {
         case regex(x, y) => Pos(x.toInt, y.toInt)
      }
   }

}


case class Cell (pos: Pos, color: Color)

object Cell {

   def fromProperty (s: String): Cell = {
      val regex = "Cell\\((Pos\\(.*\\)),(\\w+)\\)".r

      s match {
         case regex(ps, cs) =>
            Cell(Pos.fromProperty(ps), Color.fromProperty(cs))
      }
   }

}


sealed abstract class Difficulty
case object Easy extends Difficulty
case object Intermediate extends Difficulty
case object Difficult extends Difficulty


class Game (var board: Game.Board, var selection: Option[Pos])

object Game {

   type Board = List[Cell]


   val emptyBoard =
         (for (x <- (0 to 5); y <- (0 to 5))
          yield (Cell(Pos(x, y), NoColor))).toList


   def toProperties (game: Game): Properties = {
      val p = new Properties()

      val cellProp = game.board.map(_.toString).
         reduceLeft ((b, a) => b ++ "|" ++ a)
      p.setProperty("boardCurrent", cellProp)

      p.setProperty("selection", game.selection.toString)

      p
   }


   def fromProperties (props: Properties): Game = {
      val cellStrings = props.getProperty("boardCurrent").split('|')
      val cells = cellStrings.map(Cell.fromProperty).toList

      val selOpString = props.getProperty("selection")
      val regex = "Some\\((.*)\\)".r
      val selection = selOpString match {
         case regex(ps) => Some(Pos.fromProperty(ps))
         case _ => None
      }

      new Game(cells, selection)
   }


   def randomTiles (diff: Difficulty) = {
      val distMapping = diff match {
         case Easy         => List(
            (12, Red),
            (12, Blue),
            (12, Yellow)
         )
         case Intermediate => List(
            (9, Red),
            (9, Blue),
            (9, Yellow),
            (3, Violet),
            (3, Green),
            (3, Orange)
         )
         case Difficult    => List(
            (5, Red),
            (5, Blue),
            (5, Yellow),
            (1, randomPrimary),
            (5, Violet),
            (5, Green),
            (5, Orange),
            (1, randomSecondary),
            (4, Gray)
         )
      }

      Random.shuffle (distMapping flatMap { case (n, c) => List.fill (n) (c) })
   }


   def randomBoard (diff: Difficulty) =
      emptyBoard.zip(randomTiles (diff)).map { case (Cell(p, _), c) => Cell(p, c) }


   def remainingTiles (b: Board): Int =
      b.map { case (Cell(_, color)) => totalTiles(color) } sum


   /* The list of surrounding points for every possible move, in pairs:
      (hex to be leapt over, hex to be landed upon)

      This is just the raw, ideal list. These points will often be off
      the board or otherwise be a problem.
   */
   def allMoves (o: Pos): List[(Pos, Pos)] =
      o match {
         case (Pos(x, y)) if (isEven(x)) => {
            List(
               (Pos(x + 1, y - 1), Pos(x + 2, y - 1)),
               (Pos(x + 1, y    ), Pos(x + 2, y + 1)),
               (Pos(x    , y + 1), Pos(x    , y + 2)),
               (Pos(x - 1, y    ), Pos(x - 2, y + 1)),
               (Pos(x - 1, y - 1), Pos(x - 2, y - 1)),
               (Pos(x    , y - 1), Pos(x    , y - 2))
            )
         }
         case (Pos(x, y)) => {
            List(
               (Pos(x + 1, y    ), Pos(x + 2, y - 1)),
               (Pos(x + 1, y + 1), Pos(x + 2, y + 1)),
               (Pos(x    , y + 1), Pos(x    , y + 2)),
               (Pos(x - 1, y + 1), Pos(x - 2, y + 1)),
               (Pos(x - 1, y    ), Pos(x - 2, y - 1)),
               (Pos(x    , y - 1), Pos(x    , y - 2))
            )
         }
   }


   def withinBoard (b: Board) (p: Pos): Boolean =
      b.exists { case (Cell(cp, _)) => cp == p }


// Can still move is: legalMoves > 0


   def legalMoves (b: Board) (selection: Pos): List[(Pos, Pos)] = {
      allMoves (selection) filter { 
         case (_, d) => withinBoard (b) (d)
      } filter { 
         case (p1, p2) => (assessMove
            (colorAt (b) (selection))
            (colorAt (b) (p1))
            (colorAt (b) (p2))
         ) isDefined
      }
   }


   def assessMove (o: Color) (s: Color) (a: Color): Option[(Color, Color)] = 
      ((Color.subtract (o) (s)), (Color.add (o) (a))) match {
         case (Some(s2), Some(a2)) => Some((s2, a2))
         case (_       , _       ) => None
      }


   def cellAt (b: Board) (p: Pos): Cell =
      b.filter { (cell) => (cell.pos.x == p.x) && (cell.pos.y == p.y) }.head


   def colorAt (b: Board) (p: Pos): Color = (cellAt (b) (p)) color


   def mkGame (difficulty: Difficulty): Game =
      new Game(randomBoard(difficulty), None)


   def updateBoard (board: Board, start: Pos, end: Pos): Option[Board] = {
      // Get legal moves for the start pos
      val lms = legalMoves (board) (start)

      // Make sure end pos is one of those
      val move = lms.filter { case (_, a) => a == end }

      // Gather up the new three CellS
      val optCells = move match {
         case List((s, a)) => {
            val resultColors = (assessMove
               (colorAt (board) (start))
               (colorAt (board) (s))
               (colorAt (board) (a))).get

            Some((
               Cell(start, NoColor),
               Cell(s, resultColors._1),
               Cell(a, resultColors._2)
            ))
         }
         case _ => None
      }

      // Map over the board CellS, substituting those three new CellS
      optCells match {
         case Some((co, cs, ca)) => {
            Some(board.map { (bc) =>
               if (bc.pos == co.pos) co
               else if (bc.pos == cs.pos) cs
               else if (bc.pos == ca.pos) ca
               else bc
            })
         }
         case _ => None
      }
   }

}
