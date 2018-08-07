package chiffre.util

import firrtl.bitWidth
import firrtl.ir._
import firrtl.Mappers._

object removeZeroWidth {
  def apply(t: Type): Type = t match {
    case tx @ (_: GroundType | _: VectorType) => tx
    case tx: BundleType => tx.copy(fields = tx.fields filter (f => bitWidth(f.tpe) > 0)) map apply
  }
}