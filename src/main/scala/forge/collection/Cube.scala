package forge.collection

import forge._
import forge.core.Axis

trait Cube[A] {

  def axes: Set[Axis]

  def slice(indices: (String, String)*): Cube[A]

  def get(indices: (String, String)*): A

}
