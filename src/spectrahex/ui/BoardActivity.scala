package spectrahex.ui

import android.app.Activity
import android.app.Dialog
import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Matrix
import android.graphics.Paint
import android.graphics.Path
import android.graphics.RectF
import android.graphics.Region
import android.os.Bundle
import android.util.Log
import android.view.Menu
import android.view.MenuItem
import android.view.MotionEvent
import android.view.View
import android.view.Window
import java.io.FileNotFoundException
import java.io.PrintStream
import java.util.Properties

import spectrahex.game._
import spectrahex.game.Game._
import spectrahex.game.color.{Color => SymColor, _}
import spectrahex.Util._


case class DisplayHex (
   pos: Pos,
   displayPath: Path,
   centerX: Int,
   centerY: Int,
   touchRegion: Region
   )


class GameView (context: Context, game: Game)
   extends View(context) {

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


   // A group of math functions to compute the size of specific numbers
   // of hexes given screen width and height measurements

   def calcWidthRadius (numHexes: Int, screenWidth: Int): Double =
      screenWidth / ((1.5 * numHexes) + 0.5)

   def calcWidthHexMetric (numHexes: Int, radius: Double): Double = {
      val halfRadius = radius / 2
      (radius * numHexes) + (halfRadius * numHexes) + halfRadius
   }

   def calcHeightRadius (numHexes: Int, screenHeight: Int): Double =
      screenHeight / (1.74 * numHexes)

   def calcHeightHexMetric (numHexes: Int, radius: Double): Double =
      1.74 * radius * numHexes


   override def onSizeChanged (w: Int, h: Int, oldw: Int, oldh: Int) = {
      super.onSizeChanged(w, h, oldw, oldh)

      // Construct the hex geometry for drawing the entire board

      radius = List(calcWidthRadius(6, w), calcHeightRadius(6, h))
         .min.toInt

      // Values for positioning the board as a whole
      val boardOffsetX = ((w - (calcWidthHexMetric(6, radius))) / 2)
         .toInt + radius
      val boardOffsetY = (((h - (calcHeightHexMetric(6, radius))) / 2)
         + (radius * 0.5)).toInt

      // Values for positioning individual hexes
      val offsetX = (radius * 1.5).toInt
      val perpDist = (radius * 0.87).toInt
      val offsetY = perpDist * 2

      val startingPath = hex (radius)

      displayHexes =
         (for (x <- (0 to 5); y <- (0 to 5))
         yield {
            val yOddOffset = if (isEven (x)) 0 else perpDist

            val p = new Path(startingPath)

            val finalOffsetX = (x * offsetX) + boardOffsetX
            val finalOffsetY = (y * offsetY) + boardOffsetY + yOddOffset
            p.offset (finalOffsetX, finalOffsetY)

            DisplayHex (Pos(x, y), p,
               finalOffsetX, finalOffsetY, getRegion(p))
         }).toList
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
         case Orange  => 0xffff6600
         //case Gray    => Color.GRAY
         case Gray    => 0xff424242
      }


   def hexAt (p: Pos): DisplayHex = displayHexes.filter
      { (h) => (h.pos.x == p.x) && (h.pos.y == p.y) }.head


   override def onDraw (canvas: Canvas) = {
      //Log.d(logTag, "onDraw called")

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

      // Output game status to log
      Log.d(logTag, "game status-  tiles: %02d  moves: %03d"
         .format(Game.remainingTiles(game.board), game.undo.length))
   }


   def touchedHex (x: Int, y: Int): Option[Pos] = {
      val hs = displayHexes.map {
         case DisplayHex(pos, _, _, _, region) => {
            if (region.contains(x, y)) Some(pos)
            else None
         }
      }
      catOptions(hs).headOption
   }


   def adjustState (touched: Pos): Boolean = {
      game.selection match {
         case None if (colorAt (game.board) (touched) != NoColor) => {
            game.selection = Some(touched)
            true
         }
         case Some(existingSelection) if (existingSelection == touched) => {
            game.selection = None
            true
         }
         case Some(existingSelection) => Game.doMove(game, touched)
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
                  if (repaint) invalidate()
                  true
               }
               case None => false
            }
         }
         case _ => false
      }

}


class SpectraHex extends Activity {

   private var game: Game = null

   private val gameStateFile = "game.properties"


   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate (savedInstanceState)

      requestWindowFeature (Window.FEATURE_NO_TITLE)

      loadState match {
         case Some(g) => newGame(g)
         case None => newGame(Game.mkGame(Easy))
      }
   }


   def loadState: Option[Game] = {
      try {
         val fis = openFileInput(gameStateFile)
         val props = new Properties()
         props.load(fis)
         //Log.d(logTag, props.toString)
         val g = Game.fromProperties(props)
         Log.d(logTag, g.undo.toString)
         Some(g)
      }
      catch {
         case ex: FileNotFoundException => None
      }
   }


   override def onPause () {
      super.onPause()

      val props = Game.toProperties(game)
      props.setProperty("versionCode", versionCode(this).toString)

      val fos = openFileOutput(gameStateFile, Context.MODE_PRIVATE)
      val ps = new PrintStream(fos)
      val en = props.propertyNames()
      while (en.hasMoreElements()) {
         val k = en.nextElement().toString
         ps.println(k + "=" + props.getProperty(k))
      }

      ps.flush()
      ps.close()
   }


   def newGame (g: Game) = {
      game = g
      setContentView (new GameView (this, game))
   }


   override def onCreateOptionsMenu (menu: Menu): Boolean = {
      val inflater = getMenuInflater()
      inflater.inflate(R.menu.board_menu, menu)
      true
   }


   override def onOptionsItemSelected (item: MenuItem): Boolean =
      item.getItemId() match {
         case R.id.diff_easy => {
            newGame(Game.mkGame(Easy))
            true
         }
         case R.id.diff_intermediate => {
            newGame(Game.mkGame(Intermediate))
            true
         }
         case R.id.diff_difficult => {
            newGame(Game.mkGame(Difficult))
            true
         }
         case R.id.exit => {
            // Confirm this?
            finish()
            true
         }
         case R.id.about => {
            showDialog(DIALOG_ABOUT)
            true
         }
         case _ => super.onOptionsItemSelected(item)
      }


   // FIXME: Put this in a common place?
   val DIALOG_ABOUT = 0

   override protected def onCreateDialog (id: Int): Dialog =
      id match {
         case DIALOG_ABOUT => AboutDialog.create(this)
         case _ => null
      }

}
