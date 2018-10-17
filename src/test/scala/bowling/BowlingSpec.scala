package bowling

import org.scalatest.{FunSpec, Matchers}

class BowlingSpec extends FunSpec with Matchers {

  describe("A new game") {
    describe("when initialized") {
      it("should have its frame of size 0") {
        val bowling: Bowling = Bowling(List[Frame]())
        bowling.numberOfFrames should equal(0)
      }
    }
    describe("when a roll is done") {
      it("should have its frame of size 1") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(5)
        bowling.numberOfFrames should equal(1)
      }
      it("a frame should have one roll") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(5)
        bowling.numberOfFrames should equal(1)
        bowling.frames.head.numberOfRolls should equal(1)
      }
    }
  }

  describe("A roll") {
    describe("if it is the first of a frame and less or equal than 10"){
      it("should be added to the correct frame") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(5)
        bowling.frames.head.firstRoll should equal(5)
      }
    }
    describe("if it is superior than 10"){
      it("should throw an error") {
        val bowling: Bowling = Bowling(List[Frame]())
        an [Exception] should be thrownBy bowling.roll(11)
      }
    }
    describe("if it is the second of a frame and the sum of the two rolls are superior than 10"){
      it("should throw an error"){
        val bowling: Bowling = Bowling(List[Frame]())
          .roll(5)
        an [Exception] should be thrownBy bowling.roll(6)
      }
    }
  }

  describe("The frame") {
    describe("if the player did roll two times for a final score under 10 points") {
      it("should have no bonus") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(4)
        .roll(4)
        bowling.frames.head.getBonus should equal(Frame.NO_BONUS)
      }
    }
    describe("if the player hit a spare") {
      it("should have a spare bonus") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(5)
        .roll(5)
        bowling.frames.head.getBonus should equal(Frame.SPARE)
      }
      it("should have its score equal to the sum of its score and the next roll") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(5)
        .roll(5)
        .roll(4)
        bowling.frames.last.calculate(List(4)) should equal(14)
      }
    }

    describe("if the player hit a strike") {
      it("should have a strike bonus") {
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(10)
        bowling.frames.head.getBonus should equal(Frame.STRIKE)
      }
      it("should have its score equal to the sum of its score and the sum of the two next rolls"){
        val bowling: Bowling = Bowling(List[Frame]())
        .roll(10)
        .roll(4)
        .roll(6)
        bowling.frames.last.calculate(List(4, 6)) should equal(20)
      }
    }
    describe("if it is the last frame"){
      describe("if hit a spare or a strike"){
        it("should allow to roll a third time"){
          val bowling: Bowling = Bowling(List[Frame]())
          val bowlingFilled: Bowling = (1 to 10).foldLeft(bowling)((bowling, _) => bowling.roll(10))
          val lastBowling = bowlingFilled.roll(5).roll(5)
          lastBowling.frames.head.numberOfRolls should equal(3)
        }
      }
      describe("if hit a score under 10"){
        it("should not allow to roll a third time"){
          val bowling: Bowling = Bowling(List[Frame]())
          val bowlingFilled: Bowling = (1 to 20).foldLeft(bowling)((bowling, _) => bowling.roll(4))
          an [Exception] should be thrownBy bowlingFilled.roll(6)
        }
      }
    }
  }
  describe("The score"){
    describe("if the player hit only strike"){
      it("should have 300 points"){
        val bowlingFilled: Bowling = (1 to 12).foldLeft(Bowling(List[Frame]()))((bowling, _) => bowling.roll(10))
        bowlingFilled.getScore() should equal(300)
      }
    }
    describe("if the player never hits anything"){
      it("should have 0 points"){
        val bowlingFilled: Bowling = (1 to 20).foldLeft(Bowling(List[Frame]()))((bowling, _) => bowling.roll(0))
        bowlingFilled.getScore() should equal(0)
      }
    }
    describe("if the game is not finished"){
      it("with 4 rolls of 4 points and 1 roll of 5 points should display the current score that is 33"){
        val bowlingFilled: Bowling = (1 to 4).foldLeft(Bowling(List[Frame]()))((bowling, _) => bowling.roll(5))
        bowlingFilled.roll(4).getScore() should equal(33)
      }
    }
  }
  describe("A game"){
    describe("that has been finished"){
      it("should be over"){
        val bowlingFilled: Bowling = (1 to 12).foldLeft(Bowling(List[Frame]()))((bowling, _) => bowling.roll(10))
        bowlingFilled.isOver() should be (true)
      }
    }
    describe("that is not finished"){
      it("should not be over"){
        val bowlingFilled: Bowling = (1 to 5).foldLeft(Bowling(List[Frame]()))((bowling, _) => bowling.roll(10))
        bowlingFilled.isOver() should be (false)
      }
    }
  }
}
