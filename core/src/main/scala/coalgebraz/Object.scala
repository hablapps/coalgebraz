package coalgebraz

trait ObjectCore extends ToObjectOps {

  type Object[I, O, E, X] = Coalgebra[ObjectF[I, O, E, ?], X]
}
