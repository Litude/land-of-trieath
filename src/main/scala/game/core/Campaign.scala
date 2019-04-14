package game.core

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

case class Campaign(val name: String, val player: HumanPlayer, val missions: Seq[Mission])

object Campaign {
  val Directory = "definitions/campaigns"
  implicit val fromJson: Reads[Campaign] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "playerCharacters").read[ArrayBuffer[Character]].map(new HumanPlayer(_)) and
    (JsPath \ "missions").read[Seq[Mission]]
    )(Campaign.apply _)

  def readFromFile(filename: String): Option[Campaign] = {
    Try(Source.fromFile(f"${Campaign.Directory}/${filename}").getLines.mkString).map(Json.parse(_).validate[Campaign]) match {
      case Success(JsSuccess(result, _)) => Some(result)
      case _ => None
    }
  }
}

object MissionType extends Enumeration {
  val Random, Scenario = Value
}

abstract class Mission(val missionType: MissionType.Value, val enemies: Seq[Player]) {
  def createMap: Option[Map]
}
object Mission {
  val MaxEnemies = 3

  implicit val fromJson: Reads[Mission] = new Reads[Mission] {
    override def reads(json: JsValue): JsResult[Mission] = {
      (json \ "type").as[String] match {
        case "random" => json.validate[RandomMission]
        case "scenario" => json.validate[ScenarioMission]
        case _ => JsError("Invalid mission type")
      }
    }
  }
}

case class RandomMission(
    val width: Int, height: Int, val sparseness: Int, val corridors: Int, enemyList: Seq[Player]) extends Mission(MissionType.Random, enemyList
  ) {
  // need to add +1 players to account for human player
  override def createMap: Option[Map] = Some(MapGenerator.generateMap(width, height, enemyList.length + 1, sparseness, corridors))
}
object RandomMission {
  implicit val fromJson: Reads[RandomMission] = (
    (JsPath \ "width").read[Int] and
    (JsPath \ "height").read[Int] and
    (JsPath \ "sparseness").read[Int].orElse(Reads.pure(MapGenerator.DefaultSparseness)) and
    (JsPath \ "corridors").read[Int].orElse(Reads.pure(MapGenerator.DefaultCorridors)) and
    (JsPath \ "enemies").read[Seq[ArrayBuffer[Character]]].map(_.take(Mission.MaxEnemies)).map(_.map(new AIPlayer(_)))
    )(RandomMission.apply _)
}

case class ScenarioMission(val filename: String, enemyList: Seq[Player]) extends Mission(MissionType.Scenario, enemyList) {
  override def createMap: Option[Map] = Map.readFromFile(f"maps/${filename}")
}

object ScenarioMission {
  implicit val fromJson: Reads[ScenarioMission] = (
    (JsPath \ "filename").read[String] and
    (JsPath \ "enemies").read[Seq[ArrayBuffer[Character]]].map(_.take(Mission.MaxEnemies)).map(_.map(new AIPlayer(_)))
    )(ScenarioMission.apply _)
}
