package android.spectrahex

import android.content.Context
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader


object Util {

   def isEven (n: Int): Boolean = n % 2 == 0


   def catOptions[A] (l: List[Option[A]]): List[A] =
      l.filter(_.isDefined).map(_.get)


   def readInputStreamFully (is: InputStream): String = {
      val br = new BufferedReader(new InputStreamReader(is), 8192)
      val sb = new StringBuffer()
      var s = ""
      do {
         s = br.readLine()
         if (s != null) {
            sb.append(s)
            sb.append("\n")
         }
      } while (s != null)

      sb.toString()
   }


   def versionCode (context: Context): Int =
      context.getPackageManager()
      .getPackageInfo(context.getPackageName(), 0)
      .versionCode


   def versionName (context: Context): String =
      context.getPackageManager()
      .getPackageInfo(context.getPackageName(), 0)
      .versionName

}
