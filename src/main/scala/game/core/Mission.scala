package game.core

import scala.collection.mutable.ArrayBuffer

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

object MissionType extends Enumeration {
  val Random, Scenario = Value
}

abstract class Mission(val missionType: MissionType.Value, val enemies: Seq[Player]) {
  def createMap: Option[Map]
}
object Mission {
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

case class RandomMission(val width: Int, height: Int, enemyList: Seq[Player]) extends Mission(MissionType.Random, enemyList) {
  override def createMap: Option[Map] = Some(MapGenerator.generateMap(width, height, enemyList.length + 1)) //need to add +1 to players for human
}
object RandomMission {
  implicit val fromJson: Reads[RandomMission] = (
    (JsPath \ "width").read[Int] and
    (JsPath \ "height").read[Int] and
    (JsPath \ "enemies").read[Seq[ArrayBuffer[Character]]].map(_.map(new AIPlayer(_)))
    )(RandomMission.apply _)
}

case class ScenarioMission(val filename: String, enemyList: Seq[Player]) extends Mission(MissionType.Scenario, enemyList) {
  override def createMap: Option[Map] = Map.readFromFile(f"maps/${filename}")
}

object ScenarioMission {
  implicit val fromJson: Reads[ScenarioMission] = (
    (JsPath \ "filename").read[String] and
    (JsPath \ "enemies").read[Seq[ArrayBuffer[Character]]].map(_.map(new AIPlayer(_)))
    )(ScenarioMission.apply _)
}
