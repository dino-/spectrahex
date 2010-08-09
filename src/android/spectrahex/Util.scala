package android.spectrahex


object Util {

   def isEven (n: Int): Boolean = n % 2 == 0


   def catOptions[A] (l: List[Option[A]]): List[A] =
      l.filter(_.isDefined).map(_.get)

}
