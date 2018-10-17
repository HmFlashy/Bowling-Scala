package bowling

case class Frame(rolls: List[Int], index: Int){

  /**
    *
    * @return The number of rolls in the frame
    */
  def numberOfRolls: Int = {
    rolls.size
  }

  /**
    *
    * @param nextRolls The rolls that has been rolled after the current frame
    * @return The score of the current frame, with the bonuses
    */
  def calculate(nextRolls: List[Int]): Int = {
    getBonus match {
      case Frame.NO_BONUS => {
        getSum()
      }
      case Frame.SPARE => {
        getSum() + nextRolls.headOption.getOrElse(0)
      }
      case Frame.STRIKE => {
        getSum() + nextRolls.headOption.getOrElse(0) + nextRolls.lift(1).getOrElse(0)
      }
    }
  }

  /**
    *
    * @return The sum of the frame rolls
    */
  def getSum(): Int = {
    val sum = rolls.sum[Int]
    sum match {
      case correct if sum >= 0 => correct
      case _ => 0
    }
  }

  /**
    *
    * @return Get the current bonus of the frame
    */
  def getBonus(): String = {
    if(firstRoll() == 10) return Frame.STRIKE
    if(firstRoll() + secondRoll() == 10) return Frame.SPARE
    Frame.NO_BONUS
  }

  /**
    *
    * @return The current value of the first roll, -1 if it is empty
    */
  def firstRoll(): Int = {
    rolls.headOption.getOrElse(-1)
  }

  /**
    *
    * @return The current value of the second roll, -1 if it is empty
    */
  def secondRoll(): Int = {
    rolls.lift(1).getOrElse(-1)
  }

  /**
    *
    * @return The current state of the frame: See states in the companion below
    */
  def getState(): String = {
    val bonus = getBonus()
    if(index == 10) {
      bonus match {
        case Frame.STRIKE | Frame.SPARE => {
          println(bonus)
          if (numberOfRolls < 3) Frame.STATE_LAST else Frame.STATE_OVER
        }
        case _ => {
          if (numberOfRolls < 2) Frame.STATE_ACTIVE else Frame.STATE_OVER
        }
      }
    } else {
      bonus match {
        case Frame.STRIKE | Frame.SPARE => Frame.STATE_OVER
        case _ => {
          if(numberOfRolls < 2) Frame.STATE_ACTIVE else Frame.STATE_OVER
        }
      }
    }
  }

  /**
    *
    * @param number The number to check
    * @return True if the roll can be rolled in the current frame, false otherwise
    */
  def isRollCorrect(number: Int): Boolean ={
    number <= 10 && (index < 10 && (firstRoll() == -1 || firstRoll() + number <= 10) || index == 10)
  }

  /**
    *
    * @param number The number to roll
    * @return The frame with a roll added to its rolls
    * @throws java.lang.Exception if the number couldn't be rolled
    *                            !!! Could have done different with Options !!!
    */
  def roll(number: Int): Frame = {
    if(isRollCorrect(number)) Frame(rolls :+ number, index) else throw new Exception("The roll is incorrect")
  }
}

object Frame {

  def apply(index: Int): Frame = new Frame(List[Int](), index)

  /**
    * State active: when the frame is not over
    */
  val STATE_ACTIVE = "active"

  /**
    * State over: when the frame can't add any roll
    */
  val STATE_OVER = "over"

  /**
    * State last: when the frame is the last frame and can still add rolls
    */
  val STATE_LAST = "last"

  /**
    * Bonus no_bonus: A normal frame with no bonus
    */
  val NO_BONUS = "no  bonus"

  /**
    * Bonus spare: A frame that is a spare
    */
  val SPARE = "spare"

  /**
    * Bonus strike: A frame that is a strike
    */
  val STRIKE = "strike"
}
