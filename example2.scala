package example

import spray.json._
import DefaultJsonProtocol._

class Base(msg: String)

case class Thing1(msg: String, thing1Thing: String) extends Base(msg)

case class Thing2(msg: String, thing2Thing: String) extends Base(msg)

case class ServiceResponse(status: Int, result: Base)

object StandAloneBaseConversion extends DefaultJsonProtocol {

	  implicit object ColorJsonFormat extends RootJsonFormat[Base] {
	    def write(c: Base) = c match {
	    	case s: Thing2 => JsObject(("msg",JsString(s.msg)), ("thing2Thing",JsString(s.thing2Thing)))
	    	case s: Thing1 => JsObject(("msg",JsString(s.msg)), ("thing1Thing",JsString(s.thing1Thing)))
	    	case _ => serializationError(s"Could not write object $c")
	    }

	   	def read(json: JsValue) = {
				json match {
					case JsObject(map) => 
						List("msg","thing1Thing","thing2Thing").map(i => map.contains(i)).toArray match {
							case Array(true,true,false) => Thing1(map("msg").toString, map("thing1Thing").toString)
							case Array(true,false,true) => Thing2(map("msg").toString, map("thing2Thing").toString)
							case _ => deserializationError("fields invalid")
						}
					case _ => deserializationError("Base expected")
				}
			}
	  }
	}

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
		implicit val srConv = jsonFormat2(ServiceResponse)
		val res = """{"msg":"Which will this deserialize to?","thing2Thing":"Yup, thing2!"}""".parseJson.convertTo[Thing2]
		println(res)

		 val x = """{"status" : 200, "result" :{"msg":"a","thing1Thing":"t"}}""".parseJson.convertTo[ServiceResponse]
		 println(x)

	}

}

