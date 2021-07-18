package nsinha

import nsinha.Utilities.{frequencyChartMoves, getAxisFromTo, getFreqDistMoves, getTopFrequenciesMove, mapToAxes}

import scala.+:
import scala.Console.{BLUE, GREEN, RED, RESET, YELLOW}

object Evaluation {

  def evaluate(rubik: RubiksCube, d: Int, path: List[(Cube, Orientation)]): (Boolean, Map[Orientation, Int], List[(Cube, Orientation)]) = {
    val (cubeMoves, freqMapMoves) = getFreqDistMoves(rubik)
    println(freqMapMoves)

    if (freqMapMoves.isEmpty) {
      println(s"${RESET}${GREEN}A move has been found  ${path} ${RESET}")
      return (freqMapMoves.isEmpty, freqMapMoves, path)
    }
    if (d == 0) {
      return (freqMapMoves.isEmpty, freqMapMoves, path)
    }

    val topMoves = getTopFrequenciesMove(cubeMoves, freqMapMoves)
    println(s"${RESET}${BLUE}path: $path fork size:${topMoves.size}${RESET}")
    val fork = topMoves.size > 1
    val (c,t) = if (fork) {
      println(s"${RESET}${BLUE}fork started $path ${RESET}")
      val partialResultsDfs = topMoves.map { case (c, t) =>
        val cpRubik = rubik.copy()
        val axesFromTo = getAxisFromTo(t)
        val from = mapToAxes(axesFromTo._1)
        val to = mapToAxes(axesFromTo._2)
        cpRubik.makeMove(from, to, (c.currX, c.currY, c.currZ))
        println(s"${RESET}${YELLOW}partial results evaluation: ${path :+ (c,t)}${RESET}")
        val res = if (d > 3) evaluate(cpRubik, 3, path :+ (c,t)) else evaluate(cpRubik, d - 1, path :+ (c,t))
        println(s"${RESET}${YELLOW}partial results evaluation done: ${res}${RESET}")
        res
      }
      println(s"${RESET}${BLUE}fork ended${RESET}")
      val min = partialResultsDfs.map { case (r, freqs, path) => evaluateMapOfFreq(freqs) }.min
      val idx = partialResultsDfs.zipWithIndex.find { case r => evaluateMapOfFreq(r._1._2) == min }.get._2
      topMoves(idx)
    } else {
      topMoves(0)
    }
    val newPath = path :+ (c, t)
    println(s"${RESET}${RED}new move ${c}:${t}${RESET}")
    println(s"${RESET}${RED}newPath: ${newPath}${RESET}")
    val cpRubik = rubik.copy()
    val axesFromTo = getAxisFromTo(t)
    val from = mapToAxes(axesFromTo._1)
    val to = mapToAxes(axesFromTo._2)
    cpRubik.makeMove(from, to, (c.currX, c.currY, c.currZ))
    evaluate(cpRubik, d - 1, newPath)
  }

  def evaluateMapOfFreq(freqMap: Map[Orientation, Int]) = {
    val totalMoves = freqMap.values.toArray.sum
    val totalKeys = freqMap.keys.size
    2*totalKeys + totalMoves
  }
}
