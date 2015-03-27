package org.nlpr.cip.kb.corpus

import java.io.{File, PrintWriter}
import java.nio.charset.Charset

import com.google.common.io.Files

import scala.io.Source

/**
 * User: hesz
 * Date: 2015-3-23
 * Time: 12:30
 */
object freebase {

  def main(args: Array[String]) {
//    extractTypeInst()
    sampleTypeInst()
  }

  def sampleTypeInst(){
    import collection.mutable.Set
//    val entities = Set[String]()
    val types = Set[String]()
    var lastEnt = ""
    var entCount = 0
    val type_inst_path = "G:\\temp\\freebase\\embedding\\type_instance.txt"

    for(line <- Source.fromFile(type_inst_path, "utf-8").getLines){
      val terms = line.split("\t")
      if(terms.length == 2) {
        val current = terms(0)
        if(!current.equals(lastEnt))
          entCount += 1
        lastEnt = current
//        entities += terms(0)
//        types += terms(1)
      }
    }
    println(entCount)
    println(types.size)
  }

  def extractTypeInst(){
    val fb_dump_path = "F:\\temp\\freebase\\freebase-rdf-2014-06-29-00-00"
    val writer = new PrintWriter("G:\\temp\\freebase\\embedding\\type_instance.txt", "utf-8")
    var line = ""
    val patt = "<http://rdf.freebase.com/ns/(.*?)>".r

    for(line <- Source.fromFile(fb_dump_path, "utf-8").getLines){
      val terms = line.split("\t")
      if(terms.length > 3 &&
        terms(1).equals("<http://rdf.freebase.com/ns/type.object.type>") &&
        terms(0).startsWith("<http://rdf.freebase.com/ns/m.") &&
        !terms(2).equals("<http://rdf.freebase.com/ns/common.topic>")){
        val patt(sub) = terms(0)
        val patt(obj) = terms(2)
        writer.write("%s\t%s\n".format(sub, obj))
      }
    }
    writer.close()
  }
}
