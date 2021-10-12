package scalashop

import java.util.concurrent.*
import scala.collection.*

class BlurSuite extends munit.FunSuite:


  test("boxBlur") {

    val radius = 3
    val width = 5
    val height = 5
    val src = Img(width, height)
    val dst = Img(width, height)

    for (c <- 0 to width - 1) {
      for (r <- 0 to height - 1) {
        src(r, c) = c * width + r
      }
    }

    println(boxBlurKernel(src, 2, 2, 0))
    println(boxBlurKernel(src, 2, 2, 1))
    println(boxBlurKernel(src, 0, 2, 0))
    println(boxBlurKernel(src, 0, 2, 1))
    println(boxBlurKernel(src, 4, 4, 0))
    println(boxBlurKernel(src, 4, 4, 1))
  }

  test("verSkip") {

    val radius = 1
    val width = 3
    val height = 3
    val src = Img(width, height)
    val dst = Img(width, height)

    for (c <- 0 to width - 1) {
      for (r <- 0 to height - 1) {
        src(c, r) = c * width + r
      }
    }
    showImg(src)
    println(VerticalBoxBlur.blur(src, dst, 0, 3, radius))
    showImg(dst)
    println(HorizontalBoxBlur.blur(src, dst, 0, 3, radius))
    showImg(dst)
  }

  test("verBlur") {

    val radius = 1
    val width = 3
    val height = 3
    val src = Img(width, height)
    val dst = Img(width, height)

    for (c <- 0 to width - 1) {
      for (r <- 0 to height - 1) {
        src(c, r) = c * width + r
      }
    }

    showImg(src)
    VerticalBoxBlur.blur(src, dst, 0, 3, 1)
    println()
    showImg(dst)
    HorizontalBoxBlur.blur(src, dst, 0, 3, 1)
    println()
    showImg(dst)
    VerticalBoxBlur.parBlur(src, dst, 3, 1)
    println()
    showImg(dst)
    HorizontalBoxBlur.parBlur(src, dst, 3, 1)
    println()
    showImg(dst)
  }

  test("verParBlurLarge") {

    val tasks = 32
    val radius = 1
    val width = 32
    val height = 64
    val src = Img(width, height)
    val dst = Img(width, height)

    for (c <- 0 to width - 1) {
      for (r <- 0 to height - 1) {
        src(c, r) = c * width + r
      }
    }

    VerticalBoxBlur.parBlur(src, dst, tasks , radius)
    println("done")
  }

  test("horParBlurLarge") {

    val tasks = 32
    val radius = 1
    val width = 32
    val height = 64
    val src = Img(width, height)
    val dst = Img(width, height)

    for (c <- 0 to width - 1) {
      for (r <- 0 to height - 1) {
        src(c, r) = c * width + r
      }
    }

    HorizontalBoxBlur.parBlur(src, dst, tasks , radius)
    println("done")
  }

  test("verParBlur") {

    val radius = 3
    val width = 10
    val height = 3
    val src = Img(width, height)
    val dst = Img(width, height)

    VerticalBoxBlur.parBlur(src, dst, 10, 1)
  }

  def showImg (img: Img): Unit = {

    for (c <- 0 to img.width - 1) {
      for (r <- 0 to img.height - 1) {
        print(img(c, r))
        print("  ")
      }
      println()
    }
  }
