package hypermake.util

import fansi.Color._
import fansi.Attr._
import fansi._

package object printing {

  def O(s: String) = Green(s).render
  def RO(s: String) = Blue(s).render
  def C(s: String) = Yellow(s).render
  def A(s: String): String = (LightBlue ++ Bold.On)(s).render

}
