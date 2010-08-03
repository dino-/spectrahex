import scala.util.Random


sealed abstract class Color
case object NoColor extends Color
case object Red     extends Color
case object Blue    extends Color
case object Yellow  extends Color
case object Violet  extends Color
case object Green   extends Color
case object Orange  extends Color
case object Gray    extends Color

object Color {

   def totalTiles (c: Color): Int =
      c match {
         case NoColor => 0
         case Red     => 1
         case Blue    => 1
         case Yellow  => 1
         case Violet  => 2
         case Green   => 2
         case Orange  => 2
         case Gray    => 3
      }


   def colFromNumber (n: Int) =
      n match {
         case 0 => NoColor
         case 1 => Red
         case 2 => Blue
         case 3 => Yellow
         case 4 => Violet
         case 5 => Green
         case 6 => Orange
         case 7 => Gray
      }


   // A randomly chosen Red, Blue or Yellow
   def randomPrimary: Color = colFromNumber(Random.nextInt(3) + 1)

   // A randomly chosen Violet, Green or Orange
   def randomSecondary: Color = colFromNumber(Random.nextInt(3) + 4)

   // Just a Gray, randomly chosen if you want to think of it that way. :D
   def randomTertiary: Color = Gray


   def add (c1: Color) (c2: Color): Option[Color] =
      (c1, c2) match {
         case (Red,    NoColor) => Some(Red)
         case (Red,    Red)     => Some(Red)
         case (Red,    Blue)    => Some(Violet)
         case (Red,    Yellow)  => Some(Orange)
         case (Blue,   NoColor) => Some(Blue)
         case (Blue,   Red)     => Some(Violet)
         case (Blue,   Blue)    => Some(Blue)
         case (Blue,   Yellow)  => Some(Green)
         case (Yellow, NoColor) => Some(Yellow)
         case (Yellow, Red)     => Some(Orange)
         case (Yellow, Blue)    => Some(Green)
         case (Yellow, Yellow)  => Some(Yellow)
         case (Violet, NoColor) => Some(Violet)
         case (Violet, Yellow)  => Some(Gray)
         case (Violet, Violet)  => Some(Violet)
         case (Orange, NoColor) => Some(Orange)
         case (Orange, Blue)    => Some(Gray)
         case (Orange, Orange)  => Some(Orange)
         case (Green,  NoColor) => Some(Green)
         case (Green,  Red)     => Some(Gray)
         case (Green,  Green)   => Some(Green)
         case (Gray,   NoColor) => Some(Gray)
         case (Gray,   Gray)    => Some(Gray)
         case (_     , _)       => None
      }

   def subtract (c1: Color) (c2: Color): Option[Color] =
      (c1, c2) match {
         case (Red,    Red)    => Some(NoColor)
         case (Red,    Violet) => Some(Blue)
         case (Red,    Orange) => Some(Yellow)
         case (Red,    Gray)   => Some(Green)
         case (Blue,   Blue)   => Some(NoColor)
         case (Blue,   Violet) => Some(Red)
         case (Blue,   Green)  => Some(Yellow)
         case (Blue,   Gray)   => Some(Orange)
         case (Yellow, Yellow) => Some(NoColor)
         case (Yellow, Orange) => Some(Red)
         case (Yellow, Green)  => Some(Blue)
         case (Yellow, Gray)   => Some(Green)
         case (Orange, Orange) => Some(NoColor)
         case (Orange, Gray)   => Some(Blue)
         case (Green,  Green)  => Some(NoColor)
         case (Green,  Gray)   => Some(Red)
         case (Violet, Violet) => Some(NoColor)
         case (Violet, Gray)   => Some(Yellow)
         case (Gray,   Gray)   => Some(NoColor)
         case (_,      _)      => None
      }

}


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
