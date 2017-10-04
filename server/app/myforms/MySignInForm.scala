package myform

case class MySignInFormData(
  var email: String = "",
  var handle: String = "",
  var password: String = "",
  var rememberMe: Boolean = false
)

class MySignInForm(doSendmail: Boolean) extends MyForm {
  val d = MySignInFormData()
  errors += ("email" -> "")
  errors += ("handle" -> "")
  errors += ("password" -> "")
  def bind: Any = {
    d.handle = extractString("handle", Some(MyForm.handleVerifier))
    if (doSendmail) d.email = extractString("email", Some(MyForm.emailVerifier))
    else d.email = d.handle + "@nomail"
    d.password = extractString("password", Some(MyForm.passwordVerifier))
    d.rememberMe = extractBoolean("rememberMe")
    if (haserrors) return this
    d
  }
}