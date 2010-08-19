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
   touchRegion: Region
   )


class GameView (context: Context, game: Game)
   extends View(context) {

   private var displayHexes: List[DisplayHex] = List()
   private var fillPaint = new Paint
   private var strokePaint = new Paint
   private var selectionPaint = new Paint

   private val colorDustyBlue = 0xFF097286

   private val logTag = "GameView"


   // Construct the paints we'll need for all drawing

   fillPaint.setStyle(Paint.Style.FILL)
   fillPaint.setColor (colorDustyBlue)
   fillPaint.setAntiAlias (true)

   strokePaint.setStyle(Paint.Style.STROKE)
   strokePaint.setStrokeWidth (2)
   strokePaint.setColor (Color.GRAY)
   strokePaint.setAntiAlias (true)

   selectionPaint.setStyle(Paint.Style.STROKE)
   selectionPaint.setStrokeWidth (3)
   selectionPaint.setColor (Color.WHITE)
   selectionPaint.setAntiAlias (true)


   override def onSizeChanged (w: Int, h: Int, oldw: Int, oldh: Int) = {
      super.onSizeChanged(w, h, oldw, oldh)

      // Construct the hex geometry for drawing the entire board

      /* Will want to calculate radius, horiz and vertical offsets
         from screen dimensions later */
      val screenOffsetX = 47
      val screenOffsetY = 37
      val radius = 30
      val offsetX = (radius * 1.5).toInt
      val perpDist = (radius * 0.87).toInt
      val offsetY = perpDist * 2

      val startingPath = hex (radius)

      displayHexes =
         (for (x <- (0 to 5); y <- (0 to 5))
         yield {
            val yOddOffset = if (isEven (x)) 0 else perpDist

            val p = new Path(startingPath)

            p.offset (
               (x * offsetX) + screenOffsetX,
               (y * offsetY) + screenOffsetY + yOddOffset
            )

            DisplayHex (Pos(x, y), p, getRegion(p))
         }).toList
   }


   def getRegion (path: Path): Region = {
      var rf = new RectF
      path.computeBounds(rf, false)
      new Region(rf.left.toInt, rf.top.toInt, rf.right.toInt, rf.bottom.toInt)
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
         case DisplayHex(cpos@Pos(x, y), path, _) => {
            val sc = colorAt (game.board) (Pos(x, y))
            fillPaint.setColor(colorMap(sc))

            canvas.drawPath (path, fillPaint)
            canvas.drawPath (path, strokePaint)
         }
      }

      // Paint the selection

      game.selection match {
         case Some(pos) => {
            val h = hexAt (pos)
            canvas.drawPath (h.displayPath, selectionPaint)

            val moves = Game.legalMoves (game.board) (pos)
            moves.foreach {
               case (_, movePos) => {
                  val h = hexAt (movePos)
                  canvas.drawPath (h.displayPath, selectionPaint)
               }
            }
         }
         case _ => ()
      }
   }


   def touchedHex (x: Int, y: Int): Option[Pos] = {
      val hs = displayHexes.map {
         case DisplayHex(pos, _, region) => {
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
         val g = Game.fromProperties(props)
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
