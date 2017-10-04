package myform

case class MyForgotPasswordFormData(
  var email: String = ""
)

class MyForgotPasswordForm() extends MyForm {
  val d = MyForgotPasswordFormData()
  errors += ("email" -> "")
  def bind: Any = {
    d.email = extractString("email", Some(MyForm.emailVerifier))
    if (haserrors) return this
    d
  }
}