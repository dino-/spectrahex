package android.spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface

import android.spectrahex.Util


object AboutDialog {

   def create(context: Context) = {
      val builder = new AlertDialog.Builder(context)

      builder.setMessage("version " + Util.versionName(context) +
         "\nDino Morelli\ndino@ui3.info")
             .setTitle("SpectraHex")
             .setPositiveButton("OK", null)

      builder.create()
   }

}
