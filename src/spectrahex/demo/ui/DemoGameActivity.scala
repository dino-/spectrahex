package spectrahex.demo.ui

import android.app.Dialog
import android.os.Bundle
//import android.util.Log
//import java.io.FileNotFoundException
//import java.io.PrintStream
//import java.util.Properties

import spectrahex.ui._
//import spectrahex.game._
//import spectrahex.game.Game._
//import spectrahex.Util


class DemoGameActivity extends GameActivity {

   //private val gameStateFile = "game.properties"


   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate (savedInstanceState)

      showDialog(DemoGameActivity.DIALOG_DEMO)
   }


   override protected def onCreateDialog (id: Int): Dialog =
      id match {
         case DemoGameActivity.DIALOG_DEMO => DemoAlert.create(this)
         //case _ => null
         case _ => super.onCreateDialog(id)
      }

}

object DemoGameActivity {

   val DIALOG_DEMO = 2

}
