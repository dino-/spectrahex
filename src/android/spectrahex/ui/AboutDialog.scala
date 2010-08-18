package android.spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface


object AboutDialog {

   def create(context: Context) = {
      val versionString = context.getPackageManager().
         getPackageInfo(context.getPackageName(), 0).versionName

      val builder = new AlertDialog.Builder(context)

      builder.setMessage("version " + versionString +
         "\nDino Morelli\ndino@ui3.info")
             .setTitle("SpectraHex")
             .setPositiveButton("OK", null)

      builder.create()
   }

}
