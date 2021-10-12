package reductions

import scala.annotation.*
import org.scalameter.*

object ParallelParenthesesBalancingRunner:

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns := 40,
    Key.exec.maxWarmupRuns := 80,
    Key.exec.benchRuns := 120,
    Key.verbose := false
  ) withWarmer(Warmer.Default())

  def main(args: Array[String]): Unit =
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface:

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    def balance(chars: Array[Char], open: Int) : Boolean = {

      if(open < 0)
        false
      else if(chars.isEmpty) {
        open == 0
      } else {
        if(chars.head == '(') balance(chars.tail, open+1)
        else if(chars.head == ')') balance(chars.tail, open-1)
        else balance(chars.tail, open)
      }
    }

    balance(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean =

    def traverse(idx: Int, until: Int, opened: Int, closed: Int) : (Int, Int) = {
      if (idx == until || idx >= chars.length) {
        (opened, closed)
      } else {

        chars(idx) match {
          case '(' =>  traverse(idx + 1, until, opened + 1, closed)
          case ')' =>  {
            if (opened == 0) {
              traverse(idx + 1, until, opened, closed + 1)
            } else {
              traverse(idx + 1, until, opened - 1, closed)
            }
          }
          case _ => traverse(idx + 1, until, opened, closed)
        }
      }
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        def mid = (until - from) / 2 + from
        val (t1, t2) = parallel(reduce(from, mid), reduce(mid, until))
        def closed = Math.min(t1._1, t2._2)
        (t1._1 - closed + t2._1, t1._2 + t2._2 - closed)
      }
    }

    reduce(0, chars.length) == (0, 0)

  // For those who want more:
  // Prove that your reduction operator is associative!

