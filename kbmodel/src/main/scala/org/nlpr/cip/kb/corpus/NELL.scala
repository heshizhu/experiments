package org.nlpr.cip.kb.corpus

import java.io._
import java.nio.charset.Charset
import java.util.zip.GZIPInputStream

import org.apache.log4j.Logger

import scala.collection.mutable.Set
import scala.io.Source

/**
 * User: hesz
 * Date: 2015-4-6
 * Time: 9:54
 */

object NELL {
  val base_dir = "G:\\datasets\\NELL\\"


  def main(args: Array[String]) {
//    extractMention()
    filterTriple()
  }

  def filterTriple(){
    val save_path = "F:\\codes\\NiLao_PRA\\2014.pra\\NELL.08m.910.cesv_2.csv"
    val writer = new PrintWriter(save_path, "utf-8")

    val input_path = "G:\\datasets\\NELL\\NELL.08m.910.cesv.csv.gz"
    val reader : BufferedReader = new BufferedReader(new InputStreamReader(
      new GZIPInputStream(new FileInputStream(input_path))))
    writer.println(reader.readLine())
    var isOver = true
    while(isOver){
      val line = reader.readLine()
      if(line == null) isOver = false
      else{
        var terms = line.split("\t")
        var prop_str = terms(4)
        prop_str = prop_str.substring(1, prop_str.length - 1)
        val tokens = prop_str.split(",")
        if(tokens.length > 1)
          prop_str = tokens(0)
        val prop = prop_str.toDouble
        terms(4) = "[" + prop.toString + "]"
        writer.println(terms.mkString("\t"))
      }
    }
    writer.close()
    reader.close()
  }


  def extractMention(){


    val entities = Set[String]()

    val save_path = "G:\\temp\\NELL\\entity_label.txt"
    val writer = new PrintWriter(save_path, "utf-8")

    //    for(line <- Source.fromFile("G:\\temp\\NELL\\triple_entire.txt", "utf-8").getLines){
    //      println(line)
    val input_path = "G:\\datasets\\NELL\\NELL.08m.910.cesv.csv.gz"
    val reader : BufferedReader = new BufferedReader(new InputStreamReader(
      new GZIPInputStream(new FileInputStream(input_path))))
    reader.readLine()
    var isOver = true
    while(isOver){
      val line = reader.readLine()
      if(line == null) isOver = false
      else{
        val terms = line.split("\t")
        var prop_str = terms(3)
        prop_str = prop_str.substring(1, prop_str.length - 1)
        val tokens = prop_str.split(",")
        if(tokens.length > 1)
          prop_str = tokens(0)
        val prop = prop_str.toDouble
        if(prop >= 0.75){
          if(terms(0).startsWith("concept:") && !entities.contains(terms(0))){
            if(terms(8).trim.length > 0)
              writer.println("%s\t%s".format(terms(0), terms(8)))
            entities.add(terms(0))
          }
          if(terms(2).startsWith("concept:") && !entities.contains(terms(2))){
            if(terms(9).trim.length > 0)
              writer.println("%s\t%s".format(terms(2), terms(9)))
            entities.add(terms(2))
          }
        }
      }
    }
    writer.close()
    reader.close()
  }
}
