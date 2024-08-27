package zio.internal.macros

final case class Node[+Key, +A](inputs: List[Key], outputs: List[Key], value: A) {
  def map[B](f: A => B): Node[Key, B]        = copy(value = f(value))
  def mapInputs(f: Key => Key): Node[Key, A] = copy(inputs = inputs.map(f))
}
