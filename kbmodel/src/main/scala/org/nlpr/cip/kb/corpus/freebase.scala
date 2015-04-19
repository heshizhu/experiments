package org.nlpr.cip.kb.corpus

import java.io.{File, PrintWriter}
import java.nio.charset.Charset

import com.google.common.io.Files

import scala.collection.mutable.{Set, Map}
import scala.io.Source
import scala.util.matching.Regex

/**
 * User: hesz
 * Date: 2015-3-23
 * Time: 12:30
 */
object freebase {

  def main(args: Array[String]) {
//    extractTypeInst()
//    sampleTypeInst()
//      extractEntity()
//    extractMediatorInst()
//    extractMediatorTriple()
    extractTriple()
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

  def extractMediatorInst(){
    val types  = Source.fromFile("G:\\temp\\freebase\\mediator_types_filter.txt", "utf-8").getLines.toSet
    val fb_dump_path = "F:\\temp\\freebase\\freebase-rdf-2014-06-29-00-00"

    val patt = "<http://rdf.freebase.com/ns/(.*?)>".r
    val entities = scala.collection.mutable.Set[String]()
    for(line <- Source.fromFile(fb_dump_path, "utf-8").getLines){
      val terms = line.split("\t")
      if(terms.length > 3 &&
        terms(1).equals("<http://rdf.freebase.com/ns/type.object.type>") &&
        terms(0).startsWith("<http://rdf.freebase.com/ns/m.") && types.contains(terms(2))){

        val patt(sub) = terms(0)
        entities.add(sub)
      }
    }
    println("#num: " + entities.size)
    val writer = new PrintWriter("G:\\temp\\freebase\\mediator_instance.txt", "utf-8")
    for(entity <- entities)
      writer.println(entity)
    writer.close()
  }

  def extractEntity(){
    val patt = "<http://rdf.freebase.com/ns/(.*?)>".r
    var last = ""

    var count = 0
    val writer = new PrintWriter("G:\\temp\\freebase\\freebase_entity.txt", "utf-8")
    for(line <- Source.fromFile("G:\\temp\\freebase\\freebase-retain", "utf-8").getLines) {
      val terms = line.split("\t")
      if (terms.length > 3 && terms(0).startsWith("<http://rdf.freebase.com/ns/m.")) {
        val patt(sub) = terms(0)
        if(!sub.equals(last))
          writer.println(sub)
        last = sub
      }
      count += 1
      if((count % 10000000) == 0) println(count)
    }
    writer.close()
  }

  def extractMediatorTriple(){
    val mediator_entities = Source.fromFile("G:\\temp\\freebase\\mediator_instance.txt", "utf-8").getLines.toSet
    println("medictor num : " + mediator_entities.size)
    val patt = "<http://rdf.freebase.com/ns/(.*?)>".r
    val writer = new PrintWriter("G:\\temp\\freebase\\mediator_triple.txt", "utf-8")
    var count = 0
    for(line <- Source.fromFile("G:\\temp\\freebase\\freebase-retain", "utf-8").getLines) {
      val terms = line.split("\t")
      if (terms.length > 3
        && terms(0).startsWith("<http://rdf.freebase.com/ns/m.")) {
        val patt(sub) = terms(0)
        if (mediator_entities.contains(sub)) {
          writer.println(line)
        }
      }
      count += 1
      if((count % 10000000) == 0) println(count)
    }
    writer.close()
  }

  //把CVT转换成三元组形式
  def extractTriple(){
    val mediator_triples: Map[String, Set[String]] = Map[String, Set[String]]()
    val mediator_entities =
      Source.fromFile("G:\\temp\\freebase\\mediator_instance.txt", "utf-8").getLines.toSet
    println("medictor num : " + mediator_entities.size)
    val patt = "<http://rdf.freebase.com/ns/(.*?)>".r
    var count = 0
    for(line <- Source.fromFile("G:\\temp\\freebase\\freebase-retain", "utf-8").getLines) {
      val terms = line.split("\t")
      if (terms.length > 3
        && terms(0).startsWith("<http://rdf.freebase.com/ns/m.")
        && terms(2).startsWith("<http://rdf.freebase.com/ns/m.")) {
        val patt(sub) = terms(0)
        val patt(rel) = terms(1)
        val patt(obj) = terms(2)
        if (mediator_entities.contains(sub)) {
          val triple_line = "%s\t%s".format(rel, obj)
          val triples: Set[String] = if (mediator_triples.contains(sub)) mediator_triples(sub) else Set[String]()
          triples.add(triple_line)
          mediator_triples.put(sub, triples)
        }
      }
      count += 1
      if((count % 10000000) == 0) println(count)
    }
    println("mediator triple number : " + mediator_triples.size)
    count = 0
    val writer = new PrintWriter("G:\\temp\\freebase\\freebase_triple.txt", "utf-8")
    for(line <- Source.fromFile("G:\\temp\\freebase\\freebase-retain", "utf-8").getLines) {
      val terms = line.split("\t")
      if(terms.length > 3
        && terms(0).startsWith("<http://rdf.freebase.com/ns/m.")
        && terms(2).startsWith("<http://rdf.freebase.com/ns/m.")){
        val patt(sub) = terms(0)
        val patt(rel) = terms(1)
        val patt(obj) = terms(2)

        if(mediator_triples.contains(obj)){
          for(med_tri <- mediator_triples(obj)){
            val terms = med_tri.split("\t")
            if(sub != terms(1)) {
              val new_line = "%s\t%s&%s\t%s".format(sub, rel, terms(0), terms(1))
              writer.println(new_line)
            }
          }
        }
        else{
          writer.println("%s\t%s\t%s".format(sub, rel, obj))
        }
      }
      count += 1
      if((count % 10000000) == 0) println(count)
    }
    writer.close()
  }
}
