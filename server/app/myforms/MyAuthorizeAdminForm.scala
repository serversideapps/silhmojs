package myform

case class MyAuthorizeAdminFormData(
  var password: String = ""
)

class MyAuthorizeAdminForm() extends MyForm {
  val d = MyAuthorizeAdminFormData()
  errors += ("password" -> "")
  def bind: Any = {
    d.password = extractString("password", Some(MyForm.passwordVerifier))
    if (haserrors) return this
    d
  }
}