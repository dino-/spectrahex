import scala.util.Random

import colorjump.color._
import colorjump.color.Color._


case class Pos (x: Int, y: Int)


case class Cell (pos: Pos, color: Color)


sealed abstract class Difficulty
case object Easy extends Difficulty
case object Intermediate extends Difficulty
case object Difficult extends Difficulty


object Main {

   type Board = List[Cell]


   val emptyBoard = List(
      Cell(Pos(-2,  5), NoColor),
      Cell(Pos(-2,  4), NoColor),
      Cell(Pos(-2,  3), NoColor),
      Cell(Pos(-2,  2), NoColor),
      Cell(Pos(-2,  1), NoColor),
      Cell(Pos(-2,  0), NoColor),
      Cell(Pos(-2, -1), NoColor),
      Cell(Pos(-2, -2), NoColor),
      Cell(Pos(-1,  4), NoColor),
      Cell(Pos(-1,  3), NoColor),
      Cell(Pos(-1,  2), NoColor),
      Cell(Pos(-1,  1), NoColor),
      Cell(Pos(-1,  0), NoColor),
      Cell(Pos(-1, -1), NoColor),
      Cell(Pos(-1, -2), NoColor),
      Cell(Pos( 0,  4), NoColor),
      Cell(Pos( 0,  3), NoColor),
      Cell(Pos( 0,  2), NoColor),
      Cell(Pos( 0,  1), NoColor),
      Cell(Pos( 0,  0), NoColor),
      Cell(Pos( 0, -1), NoColor),
      Cell(Pos( 0, -2), NoColor),
      Cell(Pos( 0, -3), NoColor),
      Cell(Pos( 1,  3), NoColor),
      Cell(Pos( 1,  2), NoColor),
      Cell(Pos( 1,  1), NoColor),
      Cell(Pos( 1,  0), NoColor),
      Cell(Pos( 1, -1), NoColor),
      Cell(Pos( 1, -2), NoColor),
      Cell(Pos( 1, -3), NoColor),
      Cell(Pos( 2,  3), NoColor),
      Cell(Pos( 2,  2), NoColor),
      Cell(Pos( 2,  1), NoColor),
      Cell(Pos( 2,  0), NoColor),
      Cell(Pos( 2, -1), NoColor),
      Cell(Pos( 2, -2), NoColor),
      Cell(Pos( 2, -3), NoColor),
      Cell(Pos( 2, -4), NoColor),
      Cell(Pos( 3,  2), NoColor),
      Cell(Pos( 3,  1), NoColor),
      Cell(Pos( 3,  0), NoColor),
      Cell(Pos( 3, -1), NoColor),
      Cell(Pos( 3, -2), NoColor),
      Cell(Pos( 3, -3), NoColor),
      Cell(Pos( 3, -4), NoColor)
      )


   def randomTiles (diff: Difficulty) = {
      val distMapping = diff match {
         case Easy         => List((45, Color.randomPrimary _))
         case Intermediate => List((34, Color.randomPrimary _), (11, Color.randomSecondary _))
         case Difficult    =>
            List((20, Color.randomPrimary _), (20, Color.randomSecondary _), (5, Color.randomTertiary _))
      }

      Random.shuffle (distMapping flatMap { case (n, f) => List.fill (n) (f.apply) })
   }


   def randomBoard (diff: Difficulty) =
      emptyBoard.zip(randomTiles (diff)).map { case (Cell(p, _), c) => Cell(p, c) }


   def remainingTiles (b: Board): Int =
      b.map { case (Cell(_, color)) => Color.totalTiles(color) } sum


   val absMoves: List[(Pos, Pos)] = List(
      (Pos(0, 1), Pos(0, 2)),
      (Pos(1, 0), Pos(2, 0)),
      (Pos(1, -1), Pos(2, -2)),
      (Pos(0, -1), Pos(0, -2)),
      (Pos(-1, 0), Pos(-2, 0)),
      (Pos(-1, 1), Pos(-2, 2))
      )


   def transformPos (o: Pos) (p: Pos): Pos = Pos((p.x + o.x), (p.y + o.y))


   def withinBoard (b: Board) (p: Pos): Boolean =
      b.exists { case (Cell(cp, _)) => cp == p }


// Can still move is: legalMoves > 0


   def legalMoves (b: Board) (selection: Pos) = {
      def transformPair (p: Pos) (pair: (Pos, Pos)) =
         pair match {
            case (p1, p2) => ((transformPos (p) (p1)), (transformPos (p) (p2)))
         }

      absMoves.map(transformPair (selection) _) filter { 
         case (_, d) => withinBoard (b) (d)
      } filter { 
         case (p1, p2) => (assessMove
            ((cellAt (b) (selection)).color)
            ((cellAt (b) (p1)).color)
            ((cellAt (b) (p2)).color)
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
   

}
