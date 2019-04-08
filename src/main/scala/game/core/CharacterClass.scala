package game.core

import scala.io.Source

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

case class CharacterClass(
    val name: String,
    val attackPower: Int,
    val defensePower: Int,
    val movementPoints: Int,
    val hitpoints: Int,
    val image: String,
    val range: Int,
    val meleePenalty: Double,
    val projectile: Option[String],
    val attackBonus: scala.collection.immutable.Map[String, Double]) {

  override def toString: String = name
}

object CharacterClass {

  val DefaultRange = 1
  val DefaultMeleePenalty = 1.0

  implicit val fromJson: Reads[CharacterClass] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "attackPower").read[Int] and
    (JsPath \ "defensePower").read[Int] and
    (JsPath \ "movementPoints").read[Int] and
    (JsPath \ "hitpoints").read[Int] and
    (JsPath \ "image").read[String] and
    (JsPath \ "range").read[Int].orElse(Reads.pure(DefaultRange)) and
    (JsPath \ "meleePenalty").read[Double].orElse(Reads.pure(DefaultMeleePenalty)) and
    (JsPath \ "projectile").readNullable[String] and
    (JsPath \ "attackBonus").read[scala.collection.immutable.Map[String, Double]].orElse(Reads.pure(scala.collection.immutable.Map[String, Double]()))
    )(CharacterClass.apply _)

  val Values = readFromFile("definitions/character_classes.json")

  def get(name: String): CharacterClass = Values.find(_.name == name).get

  private def readFromFile(filename: String): Seq[CharacterClass] = {
    val fileContent = Source.fromFile(filename).getLines.mkString
    val json = Json.parse(fileContent)
    json.validate[Seq[CharacterClass]] match {
      case JsSuccess(result, _) => result
      case _ => Seq()
    }
  }
}
