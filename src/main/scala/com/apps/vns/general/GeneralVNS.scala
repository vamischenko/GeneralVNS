package com.apps.vns.general

import com.apps.vns.general.GeneralVNS.InputData

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object GeneralVNS {
  case class Row(machine: Int, parts: Set[Int])
  case class InputData(machineCount: Int, partCount: Int, matrix: List[Row])
  case class ResultRow(cell: Int, machines: List[Int], parts: List[Int])

  def apply(inputData: InputData): GeneralVNS = new GeneralVNS(inputData)
}

class GeneralVNS(inputData: InputData) {
  import GeneralVNS._
  private val kMax = 3
  private val nMax = 3
  val results = new ArrayBuffer[ResultRow]()
  private val currMachines = new ArrayBuffer[Int]()
  private val currParts = new ArrayBuffer[Int]()
  //private val selectedMachines = new ArrayBuffer[Int]()

  def search(criteria: Int): List[ResultRow] = {
    val start = randomSearch()
    gvns(start, criteria)
    results.toList
  }

  private def randomSearch(): Int = {
    Random.nextInt(inputData.machineCount) + 1
  }

  private def getParts(x: Int): Set[Int] = {
    println(s"x: $x")
    inputData.matrix.filter(_.machine == x).head.parts
  }

  private def isExistsInResult(forCheck: Int): Boolean = {
    results.exists { res =>
      res.machines.contains(forCheck)
    }
  }

  private def gvns(x: Int, criteria: Int) = {
    var currCriteria = 0
    var xCurr = x
    while (currCriteria < criteria) {
      println(s"try: $currCriteria")
      var k = 1
      if (!isExistsInResult(xCurr)) {
        currMachines.append(xCurr)
        currParts.appendAll(getParts(xCurr))
        //selectedMachines.append(xCurr)
        while (k <= kMax) {
          val x1 = shake(xCurr, k)
          val x2 = VND(x1, nMax)
          val (curr, kNew) = neighborhoodChange(x1, x2, k)
          k = kNew
          xCurr = curr
          if (x != xCurr && !currMachines.contains(xCurr) && !isExistsInResult(xCurr)) {
            val xCurrParts = getParts(xCurr)
            val newParts = xCurrParts.intersect(currParts.toSet)
            if (newParts.size > 1) {
              currMachines.append(xCurr)
              //selectedMachines.append(xCurr)
              currParts.clear()
              currParts.appendAll(newParts)
            }
          }
        }
        if (currParts.nonEmpty && currMachines.nonEmpty) {
          println("write cell...")
          results.append(ResultRow(currCriteria + 1, currMachines.toList.sorted, currParts.toList.sorted))
        }
      }
      currMachines.clear()
      currParts.clear()
      xCurr = randomSearch()
      currCriteria += 1
    }
  }

  private def shake(x: Int, k: Int): Int = {
    //println("Shake...")
    val neighbors = getNeighbors(x).take(kMax).map(_.machine)
    getRandomValue(neighbors, x)
  }

  private def getRandomValue(neighbors: List[Int], x: Int): Int = {
    val newRandomValue = neighbors(Random.nextInt(neighbors.size))
    if (newRandomValue != x) newRandomValue
    else getRandomValue(neighbors, x)
  }

  private def getNeighbors(x: Int): List[Row] = {
    val row = inputData.matrix.filter(_.machine == x).head
    val step1 = inputData.matrix.filter(_.machine != x).filter(_.parts.intersect(row.parts).nonEmpty)
    step1.sortWith((a, b) => a.parts.intersect(row.parts).size > b.parts.intersect(row.parts).size)
  }

  private def VND(x: Int, lMax: Int): Int = {
    //println("VND...")
    var l = 1
    var xCurr = x
    while(l <= lMax) {
      val bestNeighbor = getBestNeighbor(xCurr)
      val (curr, lNew) = neighborhoodChange(xCurr, bestNeighbor.machine, l)
      l = lNew
      xCurr = curr
    }
    xCurr
  }

  private def getBestNeighbor(x: Int): Row = {
    getNeighbors(x).head
  }

  private def neighborhoodChange(x: Int, bestNeighbor: Int, k: Int): (Int, Int) = {
    def f(curr: Int): Int = {
      val currXParts = inputData.matrix.filter(_.machine == curr).head.parts
      val intersect = currXParts.intersect(currParts.toSet)
      if (currMachines.contains(curr)) 0 else intersect.size
    }
    if (f(bestNeighbor) > f(x)) (bestNeighbor, 1)
    else (x, k + 1)
  }

}
