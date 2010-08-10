package android.spectrahex.game

import scala.util.Random

import android.spectrahex.game.color._
import android.spectrahex.game.color.Color._
import android.spectrahex.Util._


case class Pos (x: Int, y: Int)


case class Cell (pos: Pos, color: Color)


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


   def randomTiles (diff: Difficulty) = {
      val distMapping = diff match {
         case Easy         => List((36, randomPrimary _))
         case Intermediate => List(
            (27, randomPrimary _),
            (9, randomSecondary _)
         )
         case Difficult    => List(
            (16, randomPrimary _),
            (16, randomSecondary _),
            (4, randomTertiary _)
         )
      }

      Random.shuffle (distMapping flatMap { case (n, f) => List.fill (n) (f.apply) })
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

}
