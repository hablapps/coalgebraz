package org.hablapps

import scala.language.higherKinds

package object coalgebraz {
  
  type Coalgebra[F[_], S] = S => F[S]
}
