package example

import spray.json._
import DefaultJsonProtocol._

class Base(msg: String)

case class Thing1(msg: String, thing1Thing: String) extends Base(msg)

case class Thing2(msg: String, thing2Thing: String) extends Base(msg)

/** run me from the shell via: val s = new example.ExampleInheritanceSerialization(); s.show */
class ExampleInheritanceSerialization {

	implicit val thing1Conversion = jsonFormat2(Thing1)
	implicit val thing2Conversion = jsonFormat2(Thing2)

	class BaseConversion extends RootJsonFormat[Base] {
		def write(obj: Base) = obj match {
			case t1: Thing1 => t1.toJson
			case t2: Thing2 => t2.toJson
			case _ => serializationError("Could not serialize $obj, no conversion found")
		}
		
		/* Read is kind of frustrating as we need to use the fields to determine which type to turn it into */
		def read(json: JsValue) = {
			val discrimator = List(
				"thing1Thing", //Thing1 unique field
				"thing2Thing" //Thing2 unique field
			).map( d => json.asJsObject.fields.contains(d) )
			discrimator.indexOf(true) match {
				case 0 	=> json.convertTo[Thing1]
				case 1 	=> json.convertTo[Thing2]
				case _ => deserializationError("Base expected")
			}
		}
	}
	def show() {
		implicit val conversion = new BaseConversion()
		val res = """{"msg":"Which will this deserialize to?","thing2Thing":"Yup, thing2!"}""".parseJson.convertTo[Thing2]
		println(res)
	}

}

