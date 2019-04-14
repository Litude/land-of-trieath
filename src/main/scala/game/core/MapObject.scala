package game.core

import scala.io.Source
import scala.util.{Failure, Success, Try}

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

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

  val Values = MapObject.readFromFile("definitions/map_objects.json")

  def mapGeneratorObjects: Seq[MapObject] = Values.filter(!_.randomDisallowed)

  def readFromFile(filename: String): Seq[MapObject] = {
    Try(Source.fromFile(filename).getLines.mkString).map(Json.parse(_).validate[Seq[MapObject]]) match {
      case Success(JsSuccess(result, _)) => result
      case _ => Seq()
    }
  }

}
