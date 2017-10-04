package myform

import scala.concurrent._
import play.api.libs.ws.WSClient
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json.{ JsValue, JsResultException }

import javax.inject.Inject

case object BadFormRequest

case class Error(e: String)
case class Success()

class MyFormConfig @Inject() (
  configuration: play.api.Configuration,
  wsClient: play.api.libs.ws.WSClient
) {
  println("[ inject ] MyFormConfig")
  MyForm.configuration = configuration
  MyForm.wsClient = wsClient
}

object MyForm {
  var configuration: play.api.Configuration = null
  var wsClient: play.api.libs.ws.WSClient = null
  def parseV2Response(response: JsValue): Either[Error, Success] = {
    try {
      // success boolean flag is mandatory
      val success = (response \ "success").as[Boolean]
      if (success) Right(Success())
      else {
        // error codes are optional
        val errorCodes = (response \ "error-codes").asOpt[Seq[String]]
        if (errorCodes.isDefined) {
          // use the first error code, ignore the rest (if any)          
          Left(Error(errorCodes.get.head))
        } else {
          // no specific error code supplied          
          Left(Error(""))
        }
      }
    } catch {
      case ex: JsResultException =>
        // anything else doesn't meet the API definition        
        Left(Error("api-error"))
    }
  }

  def verifyV2(response: String): Future[Either[Error, Success]] = {
    println("verifying recaptcha response " + response)
    val privateKey = MyForm.configuration.getString("recaptcha.privateKey").get
    println("private key " + privateKey)
    println("ws client " + MyForm.wsClient)

    val payload = Map(
      "secret" -> Seq(privateKey),
      "response" -> Seq(response)
    )

    val futureResponse = MyForm.wsClient.url("https://www.google.com/recaptcha/api/siteverify").post(payload)

    futureResponse.map { response =>
      {
        if (response.status == play.api.http.Status.OK) {
          val responsejson = response.json
          println("recaptcha response json " + responsejson)
          parseV2Response(responsejson)
        } else {
          Left(Error("recaptcha-not-reachable"))
        }
      }
    } recover {
      case ex: java.io.IOException =>
        Left(Error("unable-to-call-recaptcha"))

      // e.g. various JSON parsing errors are possible
      case other: Any =>
        Left(Error("recaptcha-api-error"))
    }
  }

  type FieldVerifier = (String) => String

  val wrongemail = "does.not.look.like.an.email"

  def emailVerifierFunc(e: String): String = {
    val parts = e.split("@")
    if (parts.length != 2) return wrongemail
    if ((parts(0) == "") || (parts(1) == "")) return wrongemail
    ""
  }

  def passwordVerifierFunc(e: String): String = {
    if (e.length == 0) return "password.required"
    ""
  }

  def handleVerifierFunc(h: String): String = {
    if (h.length == 0) return "handle.required"
    val r = h.replaceAll("[0-9a-zA-Z]", "")
    if (r != "") return ("illegal.characters.in.handle")
    ""
  }

  val emailVerifier: FieldVerifier = (e: String) => emailVerifierFunc(e)
  val passwordVerifier: FieldVerifier = (p: String) => passwordVerifierFunc(p)
  val handleVerifier: FieldVerifier = (h: String) => handleVerifierFunc(h)
}

abstract class MyForm() {
  var m = Map[String, Seq[String]]()
  var errors = Map[String, String]()
  def get(id: String): Option[String] = {
    if (!m.contains(id)) return None
    if (m(id).size == 0) return None
    Some(m(id)(0))
  }
  var haserrors = false
  def adderror(id: String, error: String) {
    if (error != "") haserrors = true
    /*if (!errors.contains(id))*/ errors += (id -> error)
    //else errors += (id -> (errors(id) + " " + error))
  }
  def extractBoolean(id: String): Boolean = {
    adderror(id, "")
    get(id) match {
      case None => false
      case Some(value) => ((value == "true") || (value == "on"))
    }
  }
  def verify(str: String, v: Option[MyForm.FieldVerifier] = None): String = {
    if (v.isEmpty) return ""
    v.get(str)
  }
  def extractString(id: String, v: Option[MyForm.FieldVerifier] = None): String = {
    val str = get(id).getOrElse("")
    adderror(id, verify(str, v))
    str
  }
  def bind: Any
  def bindFailed(recaptchaFailed: Boolean = true): Any = null
  def bindFromRequest(implicit request: play.api.mvc.Request[play.api.mvc.AnyContent]): Any = {
    haserrors = false
    request.body match {
      case play.api.mvc.AnyContentAsFormUrlEncoded(m) => {
        this.m = m
        return bind
      }
      case _ => {
        println("Unable to bind form. Bad request.")
        return BadFormRequest
      }
    }
  }

  def bindFromRequestAndVerify(doCaptcha: Boolean = false)(implicit request: play.api.mvc.Request[play.api.mvc.AnyContent]): Future[Any] = {
    val bindResult = bindFromRequest
    if (!doCaptcha) return Future.successful(bindResult)
    bindResult match {
      case w: WithRecaptcha => MyForm.verifyV2(w.recaptcha).flatMap(result => result.fold(
        (e: myform.Error) => {
          println("bind failed to recaptcha: " + e)
          Future.successful(bindFailed())
        },
        (s: myform.Success) => {
          println("bind ok with recaptcha")
          Future.successful(bindResult)
        }
      ))
      case _ => {
        println("bind failed trivially")
        Future.successful(bindFailed(false))
      }
    }
  }
}