package nsinha

import nsinha.Utilities.{frequencyChartMoves, getAxisFromTo, getTopFrequenciesMove, mapToAxes}

import scala.+:

object Evaluation {

  def evaluate(rubik: RubiksCube, d: Int = 20, path: List[(Cube, Orientation)]): (Boolean, Map[Orientation, Int], List[(Cube, Orientation)]) = {
    val cubeMoves = rubik.findDisarrangedCubes().map({ x =>
      val derangement = Derangement.getDerangementOfCube(x, 3)
      val moves = Derangement.getMovesForOrientation(derangement)
      println(x)
      moves.foreach(println(_))
      println()
      x -> moves
    }).toMap

    val freqMapMoves = frequencyChartMoves(cubeMoves.values.toList)
    println(freqMapMoves)
    if (freqMapMoves.isEmpty || d == 0) return (freqMapMoves.isEmpty, freqMapMoves, path)

    val topMoves = getTopFrequenciesMove(cubeMoves, freqMapMoves)
    val fork = topMoves.size > 1
    val (c,t) = if (fork) {
      val partialResultsDfs = topMoves.map { case (c, t) =>
        val cpRubik = rubik.copy()
        val axesFromTo = getAxisFromTo(t)
        val from = mapToAxes(axesFromTo._1)
        val to = mapToAxes(axesFromTo._2)
        cpRubik.makeMove(from, to, (c.currX, c.currY, c.currZ))
        if (d > 3) evaluate(cpRubik, 3, List.empty) else evaluate(cpRubik, d - 1, List.empty)
      }


      val min = partialResultsDfs.map { case (r, freqs, path) => evaluateMapOfFreq(freqs) }.min
      val idx = partialResultsDfs.zipWithIndex.find { case r => evaluateMapOfFreq(r._1._2) == min }.get._2
      topMoves(idx)
    } else {
      topMoves(0)
    }
    val newPath = path.:+(c, t)
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
