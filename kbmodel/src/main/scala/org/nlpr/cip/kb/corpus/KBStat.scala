package org.nlpr.cip.kb.corpus

import scala.collection.mutable.{Set, Map}
import scala.io.Source

object KBStat {

  val base_dir = "G:\\temp\\"

  def main(args: Array[String]) {

    val nell_dir = base_dir + "NELL/"

    //    val nell_mention_entity = Map[String, Set[String]]()
    //    //统计NELL中mention个数
    //    //建立mention到NELL中entity的映射
    //    for(line <- Source.fromFile(nell_dir + "entity_mention.txt", "utf-8").getLines){
    //      val terms = line.split("\t")
    //      if(terms.length > 1 && terms(0).startsWith("concept:")){
    //        val ent = terms(0).substring(8)
    //        for(j <- 1 until terms.length){
    //          val men = terms(j).substring(1, terms(j).length - 1).toLowerCase
    //          val entities = nell_mention_entity.getOrElse(men, Set[String]())
    //          entities.add(ent)
    //          nell_mention_entity(men) = entities
    //        }
    //      }
    //    }
    //    println("mention num in NELL : " + nell_mention_entity.size)
    //
    //    //统计mention对应实体个数的频次
    //    val nell_men_frq = Map[Int, Int]()
    //    for(values <- nell_mention_entity.values){
    //      val count = nell_men_frq.getOrElse(values.size, 0) + 1
    //      nell_men_frq(values.size) = count
    //    }
    //    for((k,v) <- nell_men_frq.iterator)
    //      println(k + "\t" + v)

    val patt = "\"(.*?)\"@en".r
    val freebase_dir = base_dir + "freebase/fbentity-mention/"
    val freebase_mention_entity = Map[String, Set[String]]()
    //统计Freebase中mention个数
    //建立mention到Freebase中entity的映射
    for (line <- Source.fromFile(freebase_dir + "entity_mention.txt", "utf-8").getLines) {
      val terms = line.split("\t")
      if (terms.length > 1 && terms(0).startsWith("fb:m.")) {
        val ent = terms(0).substring(5)
        for (j <- 1 until terms.length) {
          val tmen = terms(j)
          val men = if(tmen.startsWith("\"") && tmen.endsWith("\"@en"))
            tmen.substring(1, tmen.length - 4) else tmen
          val entities = freebase_mention_entity.getOrElse(men, Set[String]())
          entities.add(ent)
          freebase_mention_entity(men) = entities
        }
      }
    }
    println("mention num in freebase : " + freebase_mention_entity.size)

    //统计mention对应实体个数的频次

    //统计多少mention是能够匹配到的，多少是不能够匹配到的


  }
}
