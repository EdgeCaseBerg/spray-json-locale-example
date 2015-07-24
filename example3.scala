package example

import spray.json._
import spray.json.DefaultJsonProtocol._

sealed trait MyTrait 

case class Class1(someField: String) extends MyTrait

case class Class2(someOtherField: Int) extends MyTrait

case class Class3(yetAnotherField: String) extends MyTrait

case class TestCase(works: Boolean, myClassWithTrait: MyTrait)

object ImplicitConversions extends DefaultJsonProtocol {
	implicit val c1Conv = jsonFormat1(Class1)
	implicit val c2Conv = jsonFormat1(Class2)
	implicit val c3Conv = jsonFormat1(Class3)

	class MyTraitConversion extends RootJsonFormat[MyTrait] {
		def write(obj: MyTrait) = obj match {
			case c : Class1 => c.toJson
			case c : Class2 => c.toJson
			case c : Class3 => c.toJson
			case _ => serializationError(s"Could not write object $obj")
		}
		
		/* Read is kind of frustrating as we need to use the fields to determine which type to turn it into */
		def read(json: JsValue) = {
			val discrimator = List(
				"someField",
				"someOtherField",
				"yetAnotherField"
			).map( d => json.asJsObject.fields.contains(d) )
			discrimator.indexOf(true) match {
				case 0 	=> json.convertTo[Class1]
				case 1 	=> json.convertTo[Class2]
				case 2  => json.convertTo[Class3]
				case _ => deserializationError("MyTrait expected")
			}
		}
	}
}

import spray.json._


object example3{
	def main(args: Array[String]): Unit = {
		import ImplicitConversions._
		implicit val mtc =new  MyTraitConversion()
		implicit val tc = jsonFormat2(TestCase)

  	val result = """{"works": true, "myClassWithTrait" : {"someOtherField" : 2}}""".parseJson.convertTo[TestCase]
  	println(result) //example.TestCase = TestCase(true,Class2(2))

	}
}
