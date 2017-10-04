package myform

case class MyGetHandleFormData(
  var handle: String = ""
)

class MyGetHandleForm() extends MyForm {
  var d = MyGetHandleFormData()
  errors += ("handle" -> "")
  def bind: Any = {
    d.handle = extractString("handle", Some(MyForm.handleVerifier))
    if (haserrors) return this
    d
  }
}