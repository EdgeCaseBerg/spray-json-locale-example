package example

import spray.json._
import DefaultJsonProtocol._
import java.util.Locale

case class Example(id: Long, lang: Locale = Locale.ENGLISH)

class ExampleLocaleSerialization {

	implicit object LocaleFormat extends JsonFormat[Locale] {
		def write(obj: Locale) = JsString(obj.toString)
		def read(json: JsValue) : Locale = json match {
			case JsString(langString) => new Locale(langString)
			case _ => deserializationError("Locale Language String Expected")
		}
	}

	def show() {
		implicit val conversion = jsonFormat2(Example)
		val obj = Example(0)
		println(obj.toJson.prettyPrint)
	}

}
