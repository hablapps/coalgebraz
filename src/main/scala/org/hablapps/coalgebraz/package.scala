package org.hablapps

import scala.language.higherKinds

package object coalgebraz {

  type Coalgebra[F[_], S] = S => F[S]

  type Costream[H, X] = Coalgebra[({type λ[α] = Stream[H, α]})#λ, X]

  type Coistream[H, X] = Coalgebra[({type λ[α] = IStream[H, α]})#λ, X]

  type Coautomata[I, O, X] = Coalgebra[({type λ[α] = Automata[I, O, α]})#λ, X]

  type Coiautomata[I, O, X] = Coalgebra[({type λ[α] = IAutomata[I, O, α]})#λ, X]

  type Costore[K, V, X] = Coalgebra[({type λ[α] = Store[K, V, α]})#λ, X]

  type Coistore[K, V, X] = Coalgebra[({type λ[α] = IStore[K, V, α]})#λ, X]

  type Coentity[I, O, C, X] = Coalgebra[({type λ[α] = Entity[I, O, C, α]})#λ, X]

  type Coientity[I, O, C, X] = Coalgebra[({type λ[α] = IEntity[I, O, C, α]})#λ, X]

  trait Void
}
