package myform

trait WithRecaptcha {
  var recaptcha: String = ""
}

case class MySignUpFormData(
  var email: String = "",
  var handle: String = "",
  var firstName: String = "",
  var lastName: String = "",
  var password: String = "",
  var passwordagain: String = ""
) extends WithRecaptcha

class MySignUpForm(doSendmail: Boolean) extends MyForm {
  var d = MySignUpFormData()
  errors += ("email" -> "")
  errors += ("handle" -> "")
  errors += ("password" -> "")
  errors += ("passwordagain" -> "")
  errors += ("recaptcha" -> "")
  def bind: Any = {
    d.handle = extractString("handle", Some(MyForm.handleVerifier))
    d.firstName = extractString("firstName")
    d.lastName = extractString("lastName")
    d.password = extractString("password", Some(MyForm.passwordVerifier))
    d.passwordagain = extractString("passwordagain")
    if (doSendmail) d.email = extractString("email", Some(MyForm.emailVerifier))
    else d.email = d.handle + "@nomail"
    println("password: " + d.password)
    println("passwordagain: " + d.passwordagain)
    val passwordsmatch = (d.password == d.passwordagain)
    println("passwordsmatch: " + passwordsmatch)
    if (!doSendmail)
      if (!passwordsmatch) adderror("passwordagain", "passwords.dont.match")
    d.recaptcha = extractString("g-recaptcha-response")
    if (haserrors) return this
    d
  }

  override def bindFailed(recaptchaFailed: Boolean = true) = {
    if (recaptchaFailed) errors += ("recaptcha" -> "required")
    this
  }
}