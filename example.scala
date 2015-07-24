package example

import spray.json._
import DefaultJsonProtocol._
import java.util.Locale

case class Example(id: Long, lang: Locale = Locale.ENGLISH)

case class BlogPost(id: Int, createdTimeEpoch: Long, published: Boolean)

case class BlogPostText(blogId: Int, lang: java.util.Locale, postText: String)


/** run me from the shell via: val s = new example.ExampleLocaleSerialization(); s.show */
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

		implicit val blogPostFormat = jsonFormat3(BlogPostText)
		val blogpostgerman = BlogPostText(0, new java.util.Locale("de"), "Ich kann nicht verstehen!")
		println(blogpostgerman.toJson)

		val res = """{"blogId":0,"lang":"de","postText":"Ich kann nicht verstehen"}""".parseJson.convertTo[BlogPostText]
		println(res)
		
	}

}
