package spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface

import spectrahex.Util


object DemoAboutDialog {

   def create(context: Context) = {
      val builder = new AlertDialog.Builder(context)

      builder.setMessage("Demo version of SpectraHex running")
             .setTitle("SpectraHex")
             .setPositiveButton("OK", null)

      builder.setMessage("version " + Util.versionName(context) +
         "\nDino Morelli\ndino@ui3.info\n\n" +
         "This is the DEMO version of SpectraHex, limited to 12 games. Please consider purchasing the full game from the Market. Have fun!")
            .setTitle(R.string.app_name_demo)
            .setIcon(R.drawable.app_icon_demo)
            .setPositiveButton("OK", null)

      builder.create()
   }

}
