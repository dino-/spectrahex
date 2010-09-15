package spectrahex.ui

import android.app.Activity
import android.content.Context
import android.graphics.Color
import android.os.Bundle
import android.view.Window
import android.webkit.WebSettings
import android.webkit.WebView
import android.widget.TextView


class HelpActivity extends Activity {

   override def onCreate (savedInstanceState: Bundle) {
      super.onCreate(savedInstanceState)

      requestWindowFeature (Window.FEATURE_NO_TITLE)

      val wv = new WebView(this)
      wv.setBackgroundColor(Color.BLACK)

      val ws = wv.getSettings
      ws.setSavePassword(false)
      ws.setSaveFormData(false)
      ws.setJavaScriptEnabled(false)
      ws.setSupportZoom(false)

      setContentView(wv)
      wv.loadUrl("file:///android_asset/help.html")
   }

}
