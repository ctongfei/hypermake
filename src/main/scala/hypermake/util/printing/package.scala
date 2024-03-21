package hypermake.util

import fansi.Attr._
import fansi.Color._
import fansi._

package object printing {

  def O(s: String) = Green(s).render

  def RO(s: String) = Blue(s).render

  def C(s: String) = Yellow(s).render

  def B(s: String) = Bold.On(s).render

  def BU(s: String) = (Bold.On ++ Underlined.On)(s).render

  def CC(s: String) = (Yellow ++ Bold.On)(s).render

  def K(s: String): String = (LightGreen ++ Bold.On)(s).render

  def Kx(s: String): String = (LightGreen)(s).render

  def V(s: String): String = (LightBlue ++ Bold.On)(s).render

  def Vx(s: String): String = (LightBlue)(s).render

}
