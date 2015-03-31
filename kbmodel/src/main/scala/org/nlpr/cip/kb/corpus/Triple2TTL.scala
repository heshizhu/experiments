package org.nlpr.cip.kb.corpus

import java.io.PrintWriter

import org.apache.log4j.Logger

import scala.io.Source
;

/**
 * User: hesz
 * Date: 2015-3-5
 * Time: 9:37
 */
object Triple2TTL {

  def main(args: Array[String]) {
    val basePath = "G:\\temp\\TransX\\fb15k_380\\data\\"

    val name = "train"
    val inputPath = "%s%s.txt".format(basePath, name)
    val outputPath = "%s%s.ttl".format(basePath, name)
    val out = new PrintWriter(outputPath)
    for(line <- Source.fromFile(inputPath).getLines){
      out.println(line.split("\t").map("<%s>".format(_)).mkString("\t") + " .")
    }
    out.close()
  }
}
