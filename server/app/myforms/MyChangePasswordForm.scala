package myform

case class MyChangePasswordFormData(
  var currentPassword: String = "",
  var newPassword: String = ""
)

class MyChangePasswordForm() extends MyForm {
  val d = MyChangePasswordFormData()
  errors += ("currentPassword" -> "")
  errors += ("newPassword" -> "")
  def bind: Any = {
    d.currentPassword = extractString("currentPassword", Some(MyForm.passwordVerifier))
    d.newPassword = extractString("newPassword", Some(MyForm.passwordVerifier))
    if (haserrors) return this
    d
  }
}