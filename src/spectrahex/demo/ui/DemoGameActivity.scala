package spectrahex.demo.ui

import android.app.Dialog
import android.content.Intent
import android.net.Uri
import android.os.Bundle
//import android.util.Log

import spectrahex.ui._
import spectrahex.game._
import spectrahex.game.Game._


class DemoGameActivity extends GameActivity {

   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate (savedInstanceState)
   }


   override protected def onCreateDialog (id: Int): Dialog =
      id match {
         case GameActivity.DIALOG_ABOUT => DemoAboutDialog.create(this)
         case _ => super.onCreateDialog(id)
      }


   override def handleNewGame (diff: Difficulty) =
      if (game.playedGames > 12) {
         showDialog(GameActivity.DIALOG_ABOUT)
      } else {
         newGame(Game.mkGame(diff, game.playedGames + 1))
      }


   def launchMarket () {
      val uri = Uri.parse("market://details?id=spectrahex.ui")
      startActivity(new Intent(Intent.ACTION_VIEW, uri))
   }

}
