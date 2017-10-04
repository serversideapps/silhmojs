package scalajsclient

abstract class JSActor {
  def receive: PartialFunction[Any, Unit]
  def !(x: Any) = receive(x)
  def self = this
}