package spectrahex.ui

import android.app.Activity
import android.app.Dialog
import android.content.Context
import android.content.Intent
import android.os.Bundle
import android.util.Log
import android.view.Menu
import android.view.MenuItem
import android.view.View
import android.view.View.OnClickListener
import android.view.Window
import android.widget.Button
import android.widget.TextView

import spectrahex.game._
import spectrahex.game.Game._
import spectrahex.Util


class GameActivity extends Activity {

   private var game: Game = null


   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate (savedInstanceState)

      requestWindowFeature (Window.FEATURE_NO_TITLE)

      Game.load(this) match {
         case Some(g) => newGame(g)
         case None => newGame(Game.mkGame(this, Normal))
      }
   }


   private val undoClickListener = new OnClickListener() {
      def onClick (v: View) {
         Game.undoMove(game)
         val gameView = findViewById(R.id.game_view)
            .asInstanceOf[GameView]
         gameView.updateControlStates
         gameView.invalidate
      }
   }


   private val redoClickListener = new OnClickListener() {
      def onClick (v: View) {
         Game.redoMove(game)
         val gameView = findViewById(R.id.game_view)
            .asInstanceOf[GameView]
         gameView.updateControlStates
         gameView.invalidate
      }
   }


   def newGame (g: Game) = {
      game = g

      setContentView(R.layout.game)

      val gameView = findViewById(R.id.game_view)
         .asInstanceOf[GameView]

      gameView.dashTiles = findViewById(R.id.dashboard_tiles)
         .asInstanceOf[TextView]
      gameView.dashMoves = findViewById(R.id.dashboard_moves)
         .asInstanceOf[TextView]

      val undoButton = findViewById(R.id.button_undo)
         .asInstanceOf[Button]
      undoButton.setOnClickListener(undoClickListener)
      gameView.undoButton = undoButton

      val redoButton = findViewById(R.id.button_redo)
         .asInstanceOf[Button]
      redoButton.setOnClickListener(redoClickListener)
      gameView.redoButton = redoButton

      gameView.initialize(game)
   }


   override def onCreateOptionsMenu (menu: Menu): Boolean = {
      val inflater = getMenuInflater()
      inflater.inflate(R.menu.board_menu, menu)
      true
   }


   override def onOptionsItemSelected (item: MenuItem): Boolean =
      item.getItemId() match {
         case R.id.diff_normal => {
            newGame(Game.mkGame(this, Normal))
            true
         }
         case R.id.diff_difficult => {
            newGame(Game.mkGame(this, Difficult))
            true
         }
         case R.id.diff_impossible => {
            newGame(Game.mkGame(this, Impossible))
            true
         }
         case R.id.help => {
            val i = new Intent(this, classOf[HelpActivity])
            startActivity(i)
            true
         }
         case R.id.exit => {
            finish()
            true
         }
         case R.id.about => {
            showDialog(GameActivity.DIALOG_ABOUT)
            true
         }
         case _ => super.onOptionsItemSelected(item)
      }


   override protected def onCreateDialog (id: Int): Dialog =
      id match {
         case GameActivity.DIALOG_ABOUT => AboutDialog.create(this)
         case GameActivity.DIALOG_WIN => WinDialog.create(this)
         case _ => null
      }

}

object GameActivity {

   val DIALOG_ABOUT = 0
   val DIALOG_WIN = 1

}
