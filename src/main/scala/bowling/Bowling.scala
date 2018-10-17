package bowling

import scala.annotation.tailrec

case class Bowling(frames: List[Frame]){

  /**
    *
    * @return The number of frames of the current game
    */
  def numberOfFrames: Int = {
    frames.size
  }

  /**
    *
    * @return True if the game is over, False otherwise
    */
  def isOver(): Boolean = {
    frames.size == 10 && frames.forall(frame => frame.getState() == Frame.STATE_OVER)
  }


  /**
    *
    * @return The current score of the game
    */
  def getScore(): Int = {


    def twoNextRolls(frames: List[Frame]): List[Int] = {
      List(frames.lift(frames.size - 2), frames.lift(frames.size - 3)).flatMap(frameOption => {
        frameOption match {
          case Some(frame) => frame.rolls
          case _ => Nil
        }
      }).slice(0, 2)
    }

    @tailrec
    def getScore(score: Int, localFrames: List[Frame]): Int = {
      val lastFrameOption = localFrames.lastOption
      lastFrameOption match {
        case Some (frame) => {
          val nextRolls = twoNextRolls(localFrames)
          getScore(score + frame.calculate(nextRolls), localFrames.filterNot(currentFrame =>  frame == currentFrame))
        }
        case None => score
      }
    }

    getScore(0, frames)
  }

  /**
    *
    * @param number The number to roll
    * @return A new state of the current game
    * @throws java.lang.Exception Can throw an exception if we try to roll if the party is over
    * @throws java.lang.Exception Can throw an exception if we try to roll a number in a frame that can't have this roll
    *
    *                   !!! I could avoid the exceptions by using the Option type and splitting the different cases !!!
    */
  def roll(number: Int): Bowling = {
    val lastFrameOption = frames.headOption
    if(lastFrameOption.isEmpty) return Bowling(Frame(numberOfFrames + 1).roll(number) :: frames)

    val lastFrame = lastFrameOption.get
    frames.size match {
      case 10 => {
        if(isOver()){
          throw new Exception("Can't roll")
        } else {
          return Bowling(lastFrame.roll(number) :: frames.tail)
        }
      }
      case _ => {
        if(lastFrame.getState() == Frame.STATE_OVER){
          return Bowling(Frame(numberOfFrames + 1).roll(number) :: frames)
        } else {
          return Bowling(lastFrame.roll(number) :: frames.tail)
        }
      }
    }
  }
}