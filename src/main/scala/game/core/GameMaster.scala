package game.core

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object GameMaster {
  private var campaign: Option[Campaign] = None
  private var currentMission = 0

  def campaignName: String = campaign.map(_.name).getOrElse("")

  def mission: Int = currentMission

  def readCampaignFromFile(filename: String): Unit = {
    val fileContent = Source.fromFile(f"definitions/campaigns/${filename}").getLines.mkString
    val json = Json.parse(fileContent)
    val result = json.validate[Campaign]
    campaign = Some(result.get)
    currentMission = 0
  }

  def createGame(): Option[Game] = {
    campaign.flatMap(currentCampaign => currentCampaign.missions.lift(currentMission) match {
      case Some(random : RandomMission) => {
        val playerList = createPlayerList(currentCampaign.player, random.enemies)
        val map = MapGenerator.generateMap(random.width, random.height, playerList.length)
        Some(new Game(playerList, map))
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
