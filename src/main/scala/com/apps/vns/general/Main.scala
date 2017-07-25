package com.apps.vns.general

import java.io.{File, PrintWriter}

import com.apps.vns.general.GeneralVNS.{InputData, ResultRow, Row}

import scala.io.Source
import Array._

object Main extends App {

  var n = 1
  getListOfFiles("src/main/resources").foreach { file =>
    println(file.getName)
    val generalVNS = GeneralVNS(readInputData(file.getPath))
    val result = generalVNS.search(5)
    printResult(s"cfp_${n}_sol.txt", result)
    println("next...")
    n += 1
  }

  def readInputData(file: String): InputData = {
    val lines = Source.fromFile(file).getLines().filter(x => !x.contains("McCormick")).toList
    val lineParts = lines.head.split(" ")
    val machineCount = lineParts(0).toInt
    val partCount = lineParts(1).toInt
    val parts = lines.tail.map { line =>
      val elems = line.split(" ").map(_.toInt)
      Row(elems.head, elems.tail.toSet)
    }
    InputData(machineCount, partCount, parts)
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def printResult(filename: String, result: List[ResultRow]) = {
    val absPath = new File(".").getAbsolutePath
    val outFile = new File(absPath.substring(0, absPath.length-1) + s"/result/$filename")
    val pw = new PrintWriter(outFile)
    result.foreach { row =>
      pw.write(s"${row.machines.mkString(" ")} - ${row.parts.mkString(" ")}\n")
    }
    pw.close()
  }

  System.exit(0)

}
