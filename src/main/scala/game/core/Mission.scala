package game.core

import scala.collection.mutable.ArrayBuffer

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object MissionType extends Enumeration {
  val Random, Scenario = Value
}

abstract class Mission(val missionType: MissionType.Value, val enemies: Seq[Player])
object Mission {
  implicit val missionFromJson: Reads[Mission] = new Reads[Mission] {
    override def reads(json: JsValue): JsResult[Mission] = {
      val result = (json \ "type").as[String] match {
        case "random" => json.as[RandomMission]
        case _ => json.as[RandomMission]
      }
      new JsSuccess(result)
    }
  }
}

case class RandomMission(val width: Int, height: Int, enemyList: Seq[Player]) extends Mission(MissionType.Random, enemyList)
object RandomMission {
  implicit val randomMissionfromJson: Reads[RandomMission] = (
    (JsPath \ "width").read[Int] and
    (JsPath \ "height").read[Int] and
    (JsPath \ "enemies").read[Seq[ArrayBuffer[Character]]].map(_.map(new AIPlayer(_)))
    )(RandomMission.apply _)
}
