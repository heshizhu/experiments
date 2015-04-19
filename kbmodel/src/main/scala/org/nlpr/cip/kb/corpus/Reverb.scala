package org.nlpr.cip.kb.corpus

import java.io.PrintWriter

import scala.io.Source
import scala.collection.mutable.{Set}

object Reverb {

  def main(args: Array[String]) {
    val mentions = Set[String]()

    val triple_writer = new PrintWriter("G:\\temp\\Reverb\\reverb_triple.txt", "utf-8")

    for (line <- Source.fromFile("E:\\datasets\\ReVerb\\reverb_clueweb_tuples-1.1.txt", "utf-8").getLines) {
      val terms = line.split("\t").map(_.toLowerCase)
      val new_line = "%s\t%s\t%s\t%s".format(terms(4), terms(5), terms(6), terms(8))
      mentions.add(terms(4))
      mentions.add(terms(6))
      triple_writer.println(new_line)
    }
    triple_writer.close()

    println("phrase num : " + mentions.size)
    val mention_writer = new PrintWriter("G:\\temp\\Reverb\\reverb_mention.txt", "utf-8")
    for(men <- mentions)
      mention_writer.println(men)
    mention_writer.close()



  }
}
