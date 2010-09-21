package spectrahex.ui

import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Matrix
import android.graphics.Paint
import android.graphics.Path
import android.graphics.RectF
import android.graphics.Region
import android.util.AttributeSet
import android.util.Log
import android.view.MotionEvent
import android.view.View
import android.widget.Button
import android.widget.TextView

import spectrahex.game._
import spectrahex.game.Game._
import spectrahex.game.color.{Color => SymColor, _}
import spectrahex.Util


case class DisplayHex (
   pos: Pos,
   displayPath: Path,
   centerX: Int,
   centerY: Int,
   touchRegion: Region
   )


class GameView (context: Context, attrs: AttributeSet)
   extends View(context, attrs) {

   private var game: Game = null

   var dashTiles: TextView = null
   var dashMoves: TextView = null
   var undoButton: Button = null
   var redoButton: Button = null

   private var displayHexes: List[DisplayHex] = List()
   private var hexFillPaint = new Paint
   private var hexBorderPaint = new Paint
   private var selFillPaint = new Paint
   private var selStrokePaint = new Paint
   private var moveFillPaint = new Paint
   private var moveStrokePaint = new Paint
   private var radius = 0


   // Set up the paints we'll need for all drawing
   List(
      hexFillPaint, hexBorderPaint,
      selFillPaint, selStrokePaint,
      moveFillPaint, moveStrokePaint
      ) .foreach(_.setAntiAlias(true))

   hexFillPaint.setStyle(Paint.Style.FILL)

   hexBorderPaint.setStyle(Paint.Style.STROKE)
   hexBorderPaint.setStrokeWidth(2)
   hexBorderPaint.setColor(Color.GRAY)

   List(selFillPaint, selStrokePaint, moveFillPaint, moveStrokePaint)
      .foreach(_.setStrokeWidth(1))

   selFillPaint.setStyle(Paint.Style.FILL)
   selFillPaint.setColor(Color.WHITE)

   selStrokePaint.setStyle(Paint.Style.STROKE)
   selStrokePaint.setColor(Color.BLACK)

   moveFillPaint.setStyle(Paint.Style.FILL)
   moveFillPaint.setColor(Color.BLACK)

   moveStrokePaint.setStyle(Paint.Style.STROKE)
   moveStrokePaint.setColor(Color.WHITE)


   def initialize (g: Game) {
      game = g
      updateControlStates
   }


   // A group of math functions to compute the size of specific numbers
   // of hexes given screen width and height measurements

   def calcBoardWidth (numHexes: Int, radius: Int): Int = {
      val halfRadius = radius / 2
      ((radius * numHexes) + (halfRadius * numHexes) + halfRadius).toInt
   }

   def calcWidth (numHexes: Int, screenWidth: Int): (Int, Int) = {
      val radius = (screenWidth / ((1.5 * numHexes) + 0.5)).toInt

      val bWidth = calcBoardWidth(numHexes, radius)

      (bWidth, radius)
   }

   def calcBoardHeight (numHexes: Int, radius: Int): Int =
      (1.74 * radius * numHexes).toInt

   def calcHeight (numHexes: Int, screenHeight: Int): (Int, Int) = {
      val radius = ((screenHeight / ((numHexes * 2) + 1)) / 0.87).toInt

      val bHeight = calcBoardHeight(numHexes, radius)

      (bHeight, radius)
   }


   override def onSizeChanged (w: Int, h: Int, oldw: Int, oldh: Int) = {
      super.onSizeChanged(w, h, oldw, oldh)

      val numHexes = 6

      // Construct the hex geometry for drawing the entire board

      val srs = List(calcWidth(numHexes, w), calcHeight(numHexes, h))
      val sr = srs.sortWith((x, y) => x._1 < y._1).head
      radius = sr._2

      // Values for positioning the board as a whole
      val boardOffsetX = ((w - (calcBoardWidth(numHexes, radius))) / 2)
         .toInt + radius
      val boardOffsetY = (((h - (calcBoardHeight(numHexes, radius))) / 2)
         + (radius * 0.5)).toInt

      // Values for positioning individual hexes
      val offsetX = (radius * 1.5).toInt
      val perpDist = (radius * 0.87).toInt
      val offsetY = perpDist * 2

      val startingPath = hex (radius)

      displayHexes =
         (for (x <- (0 to 5); y <- (0 to 5))
         yield {
            val yOddOffset = if (Util.isEven (x)) 0 else perpDist

            val p = new Path(startingPath)

            val finalOffsetX = (x * offsetX) + boardOffsetX
            val finalOffsetY = (y * offsetY) + boardOffsetY + yOddOffset
            p.offset (finalOffsetX, finalOffsetY)

            DisplayHex (Pos(x, y), p,
               finalOffsetX, finalOffsetY, getRegion(p))
         }).toList
   }


   def updateControlStates = {
      val remaining = Game.remainingTiles(game.board)
      dashTiles.setText(Util.padNumber(remaining) + " tiles")
      dashMoves.setText("moves " + Util.padNumber(game.undo.length))

      undoButton.setEnabled(! game.undo.isEmpty)
      redoButton.setEnabled(! game.redo.isEmpty)

      if (remaining < 2)
         context.asInstanceOf[GameActivity]
            .showDialog(GameActivity.DIALOG_WIN)
   }


   def getRegion (path: Path): Region = {
      var rf = new RectF
      path.computeBounds(rf, false)
      new Region(rf.left.toInt, rf.top.toInt,
         rf.right.toInt, rf.bottom.toInt)
   }


   def unitHex: Path = {
      val p = new Path

      p.moveTo ( 0.5f,  0.87f)
      p.lineTo ( 1.0f,  0.00f)
      p.lineTo ( 0.5f, -0.87f)
      p.lineTo (-0.5f, -0.87f)
      p.lineTo (-1.0f,  0.00f)
      p.lineTo (-0.5f,  0.87f)
      p.close

      p
   }


   def hex (radius: Float): Path = {
      val m = new Matrix
      m.postScale (radius, radius)

      val h = unitHex
      h.transform (m)

      h
   }


   def colorMap (symColor: SymColor): Int =
      symColor match {
         case NoColor => Color.BLACK
         case Red     => Color.RED
         case Blue    => Color.BLUE
         case Yellow  => Color.YELLOW
         //case Violet  => 0xff960096
         case Violet  => 0xff990099
         case Green   => Color.GREEN
         //case Green   => 0xff339900
         //case Orange  => 0xffff6600  // Too close to red?
         case Orange  => 0xffff8500  // halfway between these two oranges
         //case Orange  => 0xffffa500  // "web colour"
         //case Gray    => Color.GRAY
         case Gray    => 0xff424242
      }


   def hexAt (p: Pos): DisplayHex = displayHexes.filter
      { (h) => (h.pos.x == p.x) && (h.pos.y == p.y) }.head


   override def onDraw (canvas: Canvas) = {
      //Log.d(logTag, "GameView.onDraw called")

      // Paint all hexes
      displayHexes.foreach {
         case DisplayHex(cpos@Pos(x, y), path, _, _, _) => {
            val sc = colorAt (game.board) (Pos(x, y))
            hexFillPaint.setColor(colorMap(sc))

            canvas.drawPath (path, hexFillPaint)
            canvas.drawPath (path, hexBorderPaint)
         }
      }

      // Paint the selection
      val selRadius = radius / 2

      game.selection match {
         case Some(pos) => {
            val h = hexAt (pos)
            canvas.drawCircle(h.centerX, h.centerY,
               selRadius, selFillPaint)
            canvas.drawCircle(h.centerX, h.centerY,
               selRadius, selStrokePaint)

            val moves = Game.legalMoves (game.board) (pos)
            moves.foreach {
               case (_, movePos) => {
                  val h = hexAt (movePos)
                  canvas.drawCircle(h.centerX, h.centerY,
                     selRadius, moveFillPaint)
                  canvas.drawCircle(h.centerX, h.centerY,
                     selRadius, moveStrokePaint)
               }
            }
         }
         case _ => ()
      }

   }


   def touchedHex (x: Int, y: Int): Option[Pos] = {
      val hs = displayHexes.map {
         case DisplayHex(pos, _, _, _, region) => {
            if (region.contains(x, y)) Some(pos)
            else None
         }
      }
      Util.catOptions(hs).headOption
   }


   def adjustState (touched: Pos): Boolean = {
      def selectNew =
         (colorAt (game.board) (touched)) match {
            case NoColor => false
            case _ => {
               Game.setSelection(game, Some(touched))
               true
            }
         }

      game.selection match {
         case None => selectNew
         case Some(existingSelection) if (existingSelection == touched) => {
            Game.setSelection(game, None)
            true
         }
         case Some(existingSelection) => {
            var moved = false

            moved = Game.doMove(game, touched)
            if (! moved) { moved = selectNew }

            moved
         }
         case _ => false
      }
   }


   override def onTouchEvent(ev: MotionEvent) =
      ev.getAction() match {
         case MotionEvent.ACTION_DOWN => true
         case MotionEvent.ACTION_UP => {
            val x = ev.getX()
            val y = ev.getY()

            touchedHex(x.toInt, y.toInt) match {
               case Some(p) => {
                  val repaint = adjustState(p)
                  if (repaint) {
                     updateControlStates
                     invalidate
                  }
                  true
               }
               case None => false
            }
         }
         case _ => false
      }

}
