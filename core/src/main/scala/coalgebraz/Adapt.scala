package coalgebraz

import Coalgebraz._

trait AdaptDsl extends AdaptLowPriorityImplicits {
  implicit def adapt2[I1, I2, O1, O2, B1, B2, X1, X2](
      co:  Entity[I1, O1, B1, X1])(implicit
      ev0: Router[B2, I2, I1],
      ev1: Router[B2, O1, O2],
      ev2: B1  -> B2,
      ev3: X1 <-> X2): Entity[I2, O2, B2, X2] =
    co.observe(ev2).in(ev0).out(ev1).carrier(ev3)
}

trait AdaptLowPriorityImplicits {
  implicit def adapt[I1, I2, O1, O2, B1, B2, X1, X2](
      co:  Entity[I1, O1, B1, X1])(implicit
      ev0: Router[B1, I2, I1],
      ev1: Router[B1, O1, O2],
      ev2: B1  -> B2,
      ev3: X1 <-> X2): Entity[I2, O2, B2, X2] =
    co.in(ev0).out(ev1).observe(ev2).carrier(ev3)
}
