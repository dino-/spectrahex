package spectrahex.ui

import android.app.Activity
import android.content.Context
import android.os.Bundle
import android.view.Window
import android.widget.TextView


class HelpActivity extends Activity {

   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate(savedInstanceState)
      requestWindowFeature (Window.FEATURE_NO_TITLE)
      val tv = new TextView(this)
      tv.setText("Help info goes here")
      setContentView(tv)
   }

}
