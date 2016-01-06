package org.hablapps.coalgebraz

import scalaz._, Scalaz._

package object candy {
  type CandyIn = CandyIn1 \/ CandyIn2
}
