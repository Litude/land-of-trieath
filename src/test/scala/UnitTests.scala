import org.scalatest.FlatSpec
import org.scalatest.Inspectors
import org.scalatest.Matchers
import org.scalatest.OptionValues

import game.core._

class UnitTests extends FlatSpec with Matchers with Inspectors with OptionValues {

    "Character" should "apply attack bonus when attacking" in {
        val characterClass1 = CharacterClass("Test1", 15, 0, 1, 40, "test.png", 1, 1.0, None, scala.collection.immutable.Map("Test2" -> 2.0))
        val characterClass2 = CharacterClass("Test2", 15, 0, 1, 40, "test.png", 1, 1.0, None, scala.collection.immutable.Map())

        val character1 = Character(characterClass1)
        val character2 = Character(characterClass2)
        character1.attackCharacter(character2) should be (30 +- 4)
        character2.attackCharacter(character1) should be (15 +- 2)
    }

    "CharacterClass" should "read a properly formatted character class JSON file" in {
        val characterClasses = CharacterClass.readFromFile(f"${UnitTests.TestFileDirectory}/definitions/test_character.json")
        characterClasses(0).name should equal ("Warrior")
        characterClasses(0).attackPower should equal (23)
        characterClasses(0).hitpoints should equal (80)
        characterClasses(2).name should equal ("Ranger")
    }

    it should "ignore a poorly formatted character class JSON file" in {
        val characterClasses = CharacterClass.readFromFile(f"${UnitTests.TestFileDirectory}/definitions/test_character_bad.json")
        characterClasses should be (empty)
    }

    "Coordinate" should "find direction towards point" in {
        Coordinate(0, 0) directionTo Coordinate(5, 0) should equal (Direction.East)
        Coordinate(0, 0) directionTo Coordinate(0, 5) should equal (Direction.South)
        Seq(Coordinate(0, 0) directionTo Coordinate(1, 1)) should contain oneOf (Direction.South, Direction.East)
    }

    it should "calculate tile distance correcntly" in {
        Coordinate(0, 0) tileDistance Coordinate(9, 10) should equal (19)
    }

    "Direction" should "return correct opposites of directions" in {
        Direction.North.opposite should equal (Direction.South)
        Direction.South.opposite should equal (Direction.North)
        Direction.East.opposite should equal (Direction.West)
        Direction.West.opposite should equal (Direction.East)
    }

    it should "return correct relative orientations" in {
        Direction.North orientationTo Direction.South should equal (Orientation.Opposite)
        Direction.North orientationTo Direction.North should equal (Orientation.Equal)
        Direction.West orientationTo Direction.North should equal (Orientation.Clockwise)
        Direction.South orientationTo Direction.East should equal (Orientation.Counterclockwise)
    }

    "DjikstraFinder" should "find a shortest path in an empty map" in {
        val map = new Map(30, 30)
        val start = Coordinate(1, 1)
        val goal = Coordinate(21, 10)
        val pathLength = DjikstraFinder.findPath(map, Seq(), start, goal).map(_.length)
        pathLength shouldBe Some(start tileDistance goal)
    }

    it should "find reachable tiles in an empty map" in {
        val map = new Map(30, 30)
        val reachableTiles = DjikstraFinder.findReachableTiles(map, Seq(), Seq(), Coordinate(14, 14), 10)
        reachableTiles should contain (Coordinate(14, 4))
        reachableTiles should contain (Coordinate(4, 14))
        reachableTiles should contain (Coordinate(24, 14))
        reachableTiles should contain (Coordinate(14, 24))
        reachableTiles should contain noneOf (Coordinate(3, 14), Coordinate(25, 14), Coordinate(14, 25), Coordinate(0, 0), Coordinate(14, 3))
    }

    it should "give the correct distances in an empty map" in {
        val map = new Map(30, 30)
        val start = Coordinate(4, 4)
        val goals = List(Coordinate(3, 4), Coordinate(10, 10), Coordinate(20, 14))
        val distances = DjikstraFinder.findDistancesToPositions(map, Seq(), start, goals).zipWithIndex
        forAll (distances) { case (distance: Int, i: Int) => distance should be (start tileDistance goals(i)) }
    }

    it should "find a path ending at attacker range distance from target when attacking" in {
        //the distance between start and target must be > 6 from the start, naturally
        val map = new Map(30, 30)
        val start = Coordinate(5, 5)
        val target = Coordinate(19, 28)
        val range = 6
        val destination = DjikstraFinder.findPathToTarget(map, Seq(), start ,target, range).map(_.last)
        destination.value tileDistance target should equal (range)
    }

    it should "find a path to a reachable position in a regular map" in {
        val map = Map.readFromFile(f"${UnitTests.TestFileDirectory}/maps/path_test.lvl")
        map shouldBe defined
        val start = Coordinate(10, 5)
        val target = Coordinate(10, 17)
        val path = DjikstraFinder.findPath(map.value, Seq(), start, target)
        path shouldBe defined
    }

    it should "not find a path if blocked by characters" in {
        val map = Map.readFromFile(f"${UnitTests.TestFileDirectory}/maps/path_test.lvl")
        map shouldBe defined
        val start = Coordinate(10, 5)
        val target = Coordinate(10, 17)
        val character1 = Character(UnitTests.TestCharacterClass)
        val character2 = Character(UnitTests.TestCharacterClass)
        character1.position = Coordinate(6, 12)
        character2.position = Coordinate(11, 10)
        val path = DjikstraFinder.findPath(map.value, Seq(character1, character2), start, target)
        path shouldBe empty
    }

    it should "not find a path to an unreachable location" in {
        val map = Map.readFromFile(f"${UnitTests.TestFileDirectory}/maps/path_test.lvl")
        map shouldBe defined
        val start = Coordinate(10, 5)
        val target = Coordinate(1, 14)
        val path = DjikstraFinder.findPath(map.value, Seq(), start, target)
        path shouldBe empty
    }

    "MapObject" should "read object definitions from JSON file" in {
        val objects = MapObject.readFromFile(f"${UnitTests.TestFileDirectory}/definitions/test_objects.json")
        objects(0).name should equal ("Stump")
        objects(1).name should equal ("Rock")
    }
}

object UnitTests {
    val TestCharacterClass = CharacterClass("Test2", 15, 0, 1, 40, "test.png", 1, 1.0, None, scala.collection.immutable.Map())
    val TestFileDirectory = "test_files"
}
