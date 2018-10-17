import java.io.EOFException

import bowling.{Bowling, Frame}

import scala.annotation.tailrec
import scala.io.StdIn

object Main extends App {

  @tailrec
  def main(bowling: Bowling): Int = {
    println("Table: " + (0 to 9).map(index => {

      val frameOption = if(index < bowling.frames.size) bowling.frames.lift(bowling.frames.size - index - 1) else None
      frameOption match {
        case Some(frame) => "| " + frame.firstRoll() + " " + {if(frame.secondRoll() == -1) "-" else frame.secondRoll()} + " |"
        case None => "| - - |"
      }
    }).mkString)
    println("Current score: " + bowling.getScore())
    if(bowling.isOver())
      bowling.getScore()
    else{

      /**
        *
        *
        *
        * @return The number to roll
        */
      def inputRoll: Int = {
        try {
          print("Roll: ")
          val roll = StdIn.readInt()
          println(roll)
          if(roll < 0 || roll > 10){
            println("Wrong input... try again")
            inputRoll
          } else roll
        } catch {
          case _: Throwable => {
            println("Wrong input... try again")
            inputRoll
          }
        }
      }
      main(bowling.roll(inputRoll))
    }
  }

  println("Score final: " + main(Bowling(Nil)))
}