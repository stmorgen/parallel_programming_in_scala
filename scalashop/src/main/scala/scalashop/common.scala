package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable

import org.scalameter.*

/** The value of every pixel is represented as a 32 bit integer. */
type RGBA = Int

/** Returns the red component. */
def red(c: RGBA): Int = (0xff000000 & c) >>> 24

/** Returns the green component. */
def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

/** Returns the blue component. */
def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

/** Returns the alpha component. */
def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

/** Used to create an RGBA value from separate components. */
def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

/** Restricts the integer into the specified range. */
def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

/** Image is a two-dimensional matrix of pixel values. */
class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

/** Computes the blurred RGBA value of a single pixel of the input image. */
def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

  if (radius == 0) {
    src(x, y)
  } else {

    var r = 0
    var g = 0
    var b = 0
    var a = 0

    val startX = clamp(x - radius, 0, src.width - 1)
    val startY = clamp(y - radius, 0, src.height - 1)
    val endX = clamp(x + radius, 0, src.width - 1)
    val endY = clamp(y + radius, 0, src.height - 1)

    var num = 0
    var i = startX
    while ( i <= endX) {
      var j = startY
      while (j <= endY) {
        val rgba = src(i, j)
        r = r + red(rgba)
        b = b + blue(rgba)
        g = g + green(rgba)
        a = a + alpha(rgba)
        j = j + 1
        num = num + 1
      }
      i = i + 1
    }
    rgba(r/num, g/num, b/num, a/num)
  }
}

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)
