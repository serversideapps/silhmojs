package myform

case class MyResetPasswordFormData(
  var password: String = ""
)

class MyResetPasswordForm() extends MyForm {
  val d = MyResetPasswordFormData()
  errors += ("password" -> "")
  def bind: Any = {
    d.password = extractString("password", Some(MyForm.passwordVerifier))
    if (haserrors) return this
    d
  }
}