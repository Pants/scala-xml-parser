package wtf.pants.scalaxml.data

case class ContentFold[A](value: A, contents: String) {
  def flatMap[B](f: (A, String) => ContentFold[B]): ContentFold[B] = f(this.value, this.contents)
}
