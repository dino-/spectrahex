package android.spectrahex.game.color

import scala.util.Random
import scala.xml.Node


sealed abstract class Color
case object NoColor extends Color
case object Red     extends Color
case object Blue    extends Color
case object Yellow  extends Color
case object Violet  extends Color
case object Green   extends Color
case object Orange  extends Color
case object Gray    extends Color

object Color {

   def toXML (color: Color) = <Color>{color}</Color>


   def fromXML (node: Node): Color =
      node.text match {
         case "NoColor" => NoColor
         case "Red" => Red
         case "Blue" => Blue
         case "Yellow" => Yellow
         case "Violet" => Violet
         case "Green" => Green
         case "Orange" => Orange
         case "Gray" => Gray
      }


   def totalTiles (c: Color): Int =
      c match {
         case NoColor => 0
         case Red     => 1
         case Blue    => 1
         case Yellow  => 1
         case Violet  => 2
         case Green   => 2
         case Orange  => 2
         case Gray    => 3
      }


   def colFromNumber (n: Int) =
      n match {
         case 0 => NoColor
         case 1 => Red
         case 2 => Blue
         case 3 => Yellow
         case 4 => Violet
         case 5 => Green
         case 6 => Orange
         case 7 => Gray
      }


   // A randomly chosen Red, Blue or Yellow
   def randomPrimary: Color = colFromNumber(Random.nextInt(3) + 1)

   // A randomly chosen Violet, Green or Orange
   def randomSecondary: Color = colFromNumber(Random.nextInt(3) + 4)


   def add (c1: Color) (c2: Color): Option[Color] =
      (c1, c2) match {
         case (Red,    NoColor) => Some(Red)
         case (Red,    Red)     => Some(Red)
         case (Red,    Blue)    => Some(Violet)
         case (Red,    Yellow)  => Some(Orange)
         case (Red,    Green)   => Some(Gray)
         case (Blue,   NoColor) => Some(Blue)
         case (Blue,   Red)     => Some(Violet)
         case (Blue,   Blue)    => Some(Blue)
         case (Blue,   Yellow)  => Some(Green)
         case (Blue,   Orange)  => Some(Gray)
         case (Yellow, NoColor) => Some(Yellow)
         case (Yellow, Red)     => Some(Orange)
         case (Yellow, Blue)    => Some(Green)
         case (Yellow, Yellow)  => Some(Yellow)
         case (Yellow, Violet)  => Some(Gray)
         case (Violet, NoColor) => Some(Violet)
         case (Violet, Yellow)  => Some(Gray)
         case (Violet, Violet)  => Some(Violet)
         case (Orange, NoColor) => Some(Orange)
         case (Orange, Blue)    => Some(Gray)
         case (Orange, Orange)  => Some(Orange)
         case (Green,  NoColor) => Some(Green)
         case (Green,  Red)     => Some(Gray)
         case (Green,  Green)   => Some(Green)
         case (Gray,   NoColor) => Some(Gray)
         case (Gray,   Gray)    => Some(Gray)
         case (_     , _)       => None
      }

   def subtract (c1: Color) (c2: Color): Option[Color] =
      (c1, c2) match {
         case (Red,    Red)    => Some(NoColor)
         case (Red,    Blue)   => Some(NoColor)
         case (Red,    Yellow) => Some(NoColor)
         case (Red,    Violet) => Some(Blue)
         case (Red,    Orange) => Some(Yellow)
         case (Red,    Gray)   => Some(Green)
         case (Blue,   Blue)   => Some(NoColor)
         case (Blue,   Red)    => Some(NoColor)
         case (Blue,   Yellow) => Some(NoColor)
         case (Blue,   Violet) => Some(Red)
         case (Blue,   Green)  => Some(Yellow)
         case (Blue,   Gray)   => Some(Orange)
         case (Yellow, Yellow) => Some(NoColor)
         case (Yellow, Red)    => Some(NoColor)
         case (Yellow, Blue)   => Some(NoColor)
         case (Yellow, Orange) => Some(Red)
         case (Yellow, Green)  => Some(Blue)
         case (Yellow, Gray)   => Some(Violet)
         case (Orange, Orange) => Some(NoColor)
         case (Orange, Gray)   => Some(Blue)
         case (Green,  Green)  => Some(NoColor)
         case (Green,  Gray)   => Some(Red)
         case (Violet, Violet) => Some(NoColor)
         case (Violet, Gray)   => Some(Yellow)
         case (Gray,   Gray)   => Some(NoColor)
         case (_,      _)      => None
      }

}
