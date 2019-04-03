package game.core

import scala.collection.mutable.ArrayBuffer

object GameMaster {
  private var campaign: Option[Campaign] = None
  private var currentMission = 0

  def campaignName: Option[String] = campaign.map(_.name)

  def loadCampaign(filename: String): Unit = {
    campaign = Campaign.readFromFile(filename)
    currentMission = 0
  }

  def isCampaignLoaded: Boolean = campaign.isDefined

  def isCampaignWon: Boolean = {
    currentMission == campaign.map(_.missions.length).getOrElse(0)
  }

  def mission: Int = currentMission

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

  private def createPlayerList(human: Player, enemies: Seq[Player]): Seq[Player] = {
    (Seq(human) ++ enemies).map(_.copy)
  }
}
