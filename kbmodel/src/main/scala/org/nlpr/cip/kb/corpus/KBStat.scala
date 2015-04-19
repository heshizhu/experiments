package org.nlpr.cip.kb.corpus

import scala.collection.mutable.{Set, Map}
import scala.io.Source

object KBStat {

  //  val base_dir = "G:\\temp\\"
    val base_dir = ""

  def main(args: Array[String]) {

    val nell_dir = base_dir + "NELL/"

    val nell_mention_entity = Map[String, Set[String]]()
    //统计NELL中mention个数
    //建立mention到NELL中entity的映射
    for (line <- Source.fromFile(nell_dir + "entity_mention.txt", "utf-8").getLines) {
      val terms = line.split("\t").map(_.trim)
      if (terms.length > 1 && terms(0).startsWith("concept:")) {
        val ent = terms(0).substring(8)
        val mentions = terms(1).substring(1, terms(1).length - 1);
        val temp_mens = mentions.split("\" \"").map(_.toLowerCase())
        for (men <- temp_mens) {
          val entities = nell_mention_entity.getOrElse(men, Set[String]())
          entities.add(ent)
          nell_mention_entity(men) = entities
        }
      }
    }
    println("mention num in NELL : " + nell_mention_entity.size)

    //统计mention对应实体个数的频次
    val nell_men_frq = Map[Int, Int]()
    for (values <- nell_mention_entity.values) {
      val count = nell_men_frq.getOrElse(values.size, 0) + 1
      nell_men_frq(values.size) = count
    }
    for ((k, v) <- nell_men_frq.iterator)
      println(k + "\t" + v)

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
          val men = (if (tmen.startsWith("\"") && tmen.endsWith("\"@en"))
            tmen.substring(1, tmen.length - 4)
          else tmen).toLowerCase
          val entities = freebase_mention_entity.getOrElse(men, Set[String]())
          entities.add(ent)
          freebase_mention_entity(men) = entities
        }
      }
    }
    println("mention num in freebase : " + freebase_mention_entity.size)


    //统计mention对应实体个数的频次
    val freebase_men_frq = Map[Int, Int]()
    for (values <- freebase_mention_entity.values) {
      val count = freebase_men_frq.getOrElse(values.size, 0) + 1
      freebase_men_frq(values.size) = count
    }
    for ((k, v) <- freebase_men_frq.iterator)
      println(k + "\t" + v)


    val reverb_dir = base_dir + "Reverb/"
    val reverb_mentions = Source.fromFile(reverb_dir + "reverb_mention.txt", "utf-8").getLines().toSet

    //统计多少mention是能够匹配到的，多少是不能够匹配到的
    val nell_mentions = nell_mention_entity.keys.toSet
    val freebase_mentions = freebase_mention_entity.keys.toSet
    println("mention num in nell: " + nell_mentions.size)
    println("mention num in freebase: " + freebase_mentions.size)
    println("mention num in reverb: " + reverb_mentions.size)
    println("join num between nell & freeebase : " + (nell_mentions & freebase_mentions).size)
    println("join num between nell & reverb : " + (nell_mentions & reverb_mentions).size)
    println("join num between freeebase & reverb : " + (freebase_mentions & reverb_mentions).size)
    println("join num among three kb : " + (nell_mentions & freebase_mentions & reverb_mentions).size)
  }
}
