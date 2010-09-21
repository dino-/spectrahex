package spectrahex.ui

import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.content.DialogInterface.OnClickListener

import spectrahex.demo.ui.DemoGameActivity
import spectrahex.Util


object DemoAboutDialog {

   def create(context: Context) = {

      val buyClickListener = new OnClickListener() {
         def onClick (d: DialogInterface, x: Int) {
            val act = context.asInstanceOf[DemoGameActivity]
            act.launchMarket()
         }
      }

      val builder = new AlertDialog.Builder(context)


      builder.setMessage("version " + Util.versionName(context) +
         "\nDino Morelli\ndino@ui3.info\n\n" +
         "This is the DEMO version of SpectraHex, limited to 12 games. Please consider purchasing the full game from the Market. The button below will take you there. Thank you and have fun!")
            .setTitle(R.string.app_name_demo)
            .setIcon(R.drawable.app_icon_demo)
            .setPositiveButton("OK", null)
            .setNegativeButton("Buy Now", buyClickListener)

      builder.create()
   }

}
