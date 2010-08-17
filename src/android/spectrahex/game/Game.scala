package android.spectrahex.game

import scala.util.Random
import scala.xml.Node

import android.spectrahex.game.color._
import android.spectrahex.game.color.Color._
import android.spectrahex.Util._


case class Pos (x: Int, y: Int)

object Pos {

   def toXML (pos: Pos) =
      <Pos><x>{pos.x}</x><y>{pos.y}</y></Pos>


   def fromXML (node: Node): Pos = {
      val x = (node \ "x").text.toInt
      val y = (node \ "y").text.toInt
      Pos(x, y)
   }

}


case class Cell (pos: Pos, color: Color)

object Cell {

   def toXML (cell: Cell) =
      <Cell>{Pos.toXML(cell.pos)}{Color.toXML(cell.color)}</Cell>


   def fromXML (node: Node): Cell = {
      val pos = Pos.fromXML((node \ "Pos").head)
      val color = Color.fromXML((node \ "Color").head)
      Cell(pos, color)
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


   def toXML (game: Game) =
      <Game>
         <Board>
            {game.board.map(Cell.toXML(_))}
         </Board>
         {game.selection match {
            case Some(pos) => <Selection>{Pos.toXML(pos)}</Selection>
            case None => <Selection />
         }}
      </Game>


   def fromXML (node: Node): Game = {
      val cells = (node \ "Board" \ "Cell").map(Cell.fromXML(_)).toList
      val nodeSelection = (node \ "Selection" \ "Pos").headOption
      val optSelection = nodeSelection match {
         case Some(n) => Some(Pos.fromXML(n))
         case None => None
      }
      new Game(cells, optSelection)
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
