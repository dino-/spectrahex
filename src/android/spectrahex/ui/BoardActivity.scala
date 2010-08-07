package android.spectrahex.ui

import android.app.Activity
import android.content.Context
import android.graphics.Canvas
import android.graphics.Color
import android.graphics.Matrix
import android.graphics.Paint
import android.graphics.Path
import android.os.Bundle
import android.util.DisplayMetrics
//import android.util.Log
import android.view.View


class DrawableView (context: Context) extends View(context) {

   private var hexPaths: List[((Int, Int), Path)] = List()
   private var fillPaint = new Paint
   private var strokePaint = new Paint

   private val colorDustyBlue = 0xFF097286


   def this (context: Context, screenWidth: Int, screenHeight: Int) = {
      this (context)

      // Construct the paints we'll need for all drawing

      fillPaint.setStyle(Paint.Style.FILL)
      fillPaint.setColor (colorDustyBlue)
      fillPaint.setAntiAlias (true)

      strokePaint.setStyle(Paint.Style.STROKE)
      strokePaint.setStrokeWidth (3)
      strokePaint.setColor (Color.GRAY)
      strokePaint.setAntiAlias (true)


      // Construct the hex geometry for drawing the entire board

      /* Will want to calculate radius, horiz and vertical offsets
         from screen dimensions later */
      val screenOffsetX = 47
      val screenOffsetY = 45
      val radius = 30
      val offsetX = (radius * 1.5).toInt
      val perpDist = (radius * 0.87).toInt
      val offsetY = perpDist * 2

      val startingPath = hex (radius)

      hexPaths =
         (for (x <- (0 to 5); y <- (0 to 5))
         yield {
            val yOddOffset = if (isEven (x)) 0 else perpDist

            val p = new Path(startingPath)

            p.offset (
               (x * offsetX) + screenOffsetX,
               (y * offsetY) + screenOffsetY + yOddOffset
            )

            ((x, y), p)
         }).toList
   }


   def isEven (n: Int): Boolean = n % 2 == 0


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


   override def onDraw (canvas: Canvas) = {
      hexPaths.foreach {
         case ((_, _), path) => {
            canvas.drawPath (path, fillPaint)
            canvas.drawPath (path, strokePaint)
         }
      }

      invalidate()
   }

}


class SpectraHex extends Activity {

   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate (savedInstanceState)

      val dm = new DisplayMetrics
      getWindowManager().getDefaultDisplay().getMetrics(dm)

      val dv = new DrawableView (this, dm.widthPixels, dm.heightPixels)
      setContentView (dv)
   }

}
