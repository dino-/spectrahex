package spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface


object WinDialog {

   def create(context: Context) = {
      val builder = new AlertDialog.Builder(context)

      builder.setMessage("You have successfully completed the game!")
             .setTitle("Good job!")
             .setPositiveButton("OK", null)

      builder.create()
   }

}
