package game.core

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Reads._

object GameMaster {
  private var campaign: Option[Campaign] = None
  private var currentMission = 0

  def campaignName: String = campaign.map(_.name).getOrElse("")

  def mission: Int = currentMission

  def readCampaignFromFile(filename: String): Unit = {
    val fileContent = Source.fromFile(f"definitions/campaigns/${filename}").getLines.mkString
    val json = Json.parse(fileContent)
    campaign = json.validate[Campaign] match {
      case JsSuccess(result, _) => Some(result)
      case _ => None
    }
    currentMission = 0
  }

  def createGame(): Option[Game] = {
    campaign.flatMap(currentCampaign => currentCampaign.missions.lift(currentMission) match {
      case Some(mission) => {
        val playerList = createPlayerList(currentCampaign.player, mission.enemies)
        mission.createMap.map(new Game(playerList, _))
      }
      case _ => None
    })
  }

  def missionWon(newCharacters: ArrayBuffer[Character]): Unit = {
    currentMission += 1
    campaign.foreach(currentCampaign => {
      currentCampaign.player.characters.clear
      currentCampaign.player.characters ++= newCharacters
    })
  }

  def isWon: Boolean = {
    currentMission == campaign.map(_.missions.length).getOrElse(0)
  }

  private def createPlayerList(human: Player, enemies: Seq[Player]): Seq[Player] = {
    (Seq(human) ++ enemies).map(_.copy)
  }
}

case class Campaign(val name: String, val player: HumanPlayer, val missions: Seq[Mission])

object Campaign {
  implicit val fromJson: Reads[Campaign] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "playerCharacters").read[ArrayBuffer[Character]].map(new HumanPlayer(_)) and
    (JsPath \ "missions").read[Seq[Mission]]
    )(Campaign.apply _)
}
