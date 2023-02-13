package hypermake.util

import fansi.Color._
import fansi.Attr._
import fansi._

package object printing {

  def O(s: String) = Green(s).render
  def RO(s: String) = Blue(s).render
  def C(s: String) = Yellow(s).render
  def B(s: String) = Bold.On(s).render
  def CC(s: String) = (Yellow ++ Bold.On)(s).render
  def K(s: String): String = (LightGreen ++ Bold.On)(s).render
  def V(s: String): String = (LightBlue ++ Bold.On)(s).render

}
