package game.core

import scala.io.Source

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class MapObject(name: String, tiles: List[List[Option[Int]]], randomDisallowed: Boolean) {
  def width: Int = tiles.length
  def height: Int = tiles(0).length
}

object MapObject {
  implicit val optionReader: Reads[Option[Int]] = new Reads[Option[Int]] {
    override def reads(json: JsValue): JsResult[Option[Int]] = json.validateOpt[Int]
  }

  implicit val fromJson: Reads[MapObject] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "tiles").read[List[List[Option[Int]]]] and
    (JsPath \ "randomDisallowed").read[Boolean].orElse(Reads.pure(false))
    )(MapObject.apply _)

  def readFromFile(filename: String): Seq[MapObject] = {
    val fileContent = Source.fromFile(filename).getLines.mkString
    val json = Json.parse(fileContent)
    val testResult = json.validate[Seq[MapObject]]
    testResult.get
  }
}
