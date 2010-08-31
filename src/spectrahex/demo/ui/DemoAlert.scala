package spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface


object DemoAlert {

   def create(context: Context) = {
      val builder = new AlertDialog.Builder(context)

      builder.setMessage("Demo version of SpectraHex running")
             .setTitle("SpectraHex")
             .setPositiveButton("OK", null)

      builder.create()
   }

}
