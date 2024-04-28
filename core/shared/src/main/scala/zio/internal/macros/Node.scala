package zio.internal.macros

final case class Node[+Key, +A](inputs: List[Key], outputs: List[Key], value: A, isEnv: Boolean = true) {
  def map[B](f: A => B): Node[Key, B] = copy(value = f(value))

  def getInputs(): List[Key] =  if (isEnv) List.empty else inputs
}
