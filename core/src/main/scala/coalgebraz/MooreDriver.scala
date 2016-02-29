package coalgebraz

trait MooreDriver {

  def run[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O = unfold(m)(x)

  def unfold[I, O, X](m: Moore[I, O, X])(x: X): List[I] => O = {
    val MooreF(o, nxt) = m(x)
    s => if (s.isEmpty) o else unfold(m)(nxt(s.head))(s.tail)
  }
}
