package org.nlpr.cip.kb.corpus

import java.io.PrintWriter

import scala.collection.mutable.{Set, Map}
import scala.io.Source

object KBStat {

//    val base_dir = "G:\\temp\\"
    val base_dir = "G:\\temp\\"

  def main(args: Array[String]) {

//    extractMentions()

    extractTriple()


  }

  def extractTriple(){
    for(corpus <- List("joint_corpus", "joint_corpus_added")){
      val dir = base_dir + "kbjoint/" + corpus + "/"
      val mentions = Source.fromFile(dir + "mentions.txt", "utf-8").getLines().toSet
      var count = 0
      //抽取reverb
//      val writer_rv = new PrintWriter(dir + "triples_reverb.txt", "utf-8")
//      for (line <- Source.fromFile(base_dir + "Reverb/reverb_triple.txt", "utf-8").getLines) {
//        val terms = line.split("\t").map(_.trim)
//        if(mentions.contains(terms(0)) && mentions.contains(terms(2))){
//          writer_rv.println(line)
//          count += 1
//        }
//      }
//      writer_rv.close()
//      println(corpus + " reverb :" + count)

      count = 0
      //抽取NELL，首先找到对应的Entity
      val ent_nl = Set[String]()
      for (line <- Source.fromFile(base_dir + "NELL/entity_mention.txt", "utf-8").getLines) {
        val terms = line.split("\t").map(_.trim)
        if (terms.length > 1 && terms(0).startsWith("concept:")) {
          val ent = terms(0)
          val ment = terms(1).substring(1, terms(1).length - 1)
          for (temp <- ment.split("\" \"").map(_.toLowerCase())){
            if(mentions.contains(temp))
              ent_nl.add(ent)
          }
        }
      }
      println("entity number :" + ent_nl.size)
      val writer_nl = new PrintWriter(dir + "triples_nell.txt", "utf-8")
      for (line <- Source.fromFile(base_dir + "NELL/filtered/triple_entire.txt", "utf-8").getLines) {
        val terms = line.split("\t")
        if(ent_nl.contains(terms(0)) && ent_nl.contains(terms(2))){
          writer_nl.println(line)
          count += 1
        }
      }
      writer_nl.close()

    }
  }

  def extractMentions(){
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
    //    val nell_men_frq = Map[Int, Int]()
    //    for (values <- nell_mention_entity.values) {
    //      val count = nell_men_frq.getOrElse(values.size, 0) + 1
    //      nell_men_frq(values.size) = count
    //    }
    //    for ((k, v) <- nell_men_frq.iterator)
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
    //    val freebase_men_frq = Map[Int, Int]()
    //    for (values <- freebase_mention_entity.values) {
    //      val count = freebase_men_frq.getOrElse(values.size, 0) + 1
    //      freebase_men_frq(values.size) = count
    //    }
    //    for ((k, v) <- freebase_men_frq.iterator)
    //      println(k + "\t" + v)


    val reverb_dir = base_dir + "Reverb/"

    val rv_mens = Source.fromFile(reverb_dir + "reverb_mention.txt", "utf-8").getLines().toSet
    val nl_mens = nell_mention_entity.keys.toSet
    val fb_mens = freebase_mention_entity.keys.toSet

    //统计多少mention是能够匹配到的，多少是不能够匹配到的
    //    println("mention num in nell: " + nell_mentions.size)
    //    println("mention num in freebase: " + freebase_mentions.size)
    //    println("mention num in reverb: " + reverb_mentions.size)
    //    println("join num between nell & freeebase : " + (nell_mentions & freebase_mentions).size)
    //    println("join num between nell & reverb : " + (nell_mentions & reverb_mentions).size)
    //    println("join num between freeebase & reverb : " + (freebase_mentions & reverb_mentions).size)
    //    println("join num among three kb : " + (nell_mentions & freebase_mentions & reverb_mentions).size)

    //至少出现在两个知识库中的mention集合
    val mens_in_two_kb = (nl_mens & fb_mens) | (nl_mens & rv_mens) | (fb_mens & rv_mens)
    println("mention size in two or more kbs: " + mens_in_two_kb.size)
    val writer_two_kb = new PrintWriter(base_dir + "mentions_in_two.txt", "utf-8")
    for(men <- mens_in_two_kb)
      writer_two_kb.println(men)
    writer_two_kb.close()

    //每个部分加20%的mention
    //FB
    var mens_addes = Set[String]()


    {
      val mens_join_fb = mens_in_two_kb & fb_mens
      val remained = fb_mens -- mens_join_fb
      val added_fb = mens_join_fb | remained.take((mens_join_fb.size * 0.2).toInt)
      println("freebase added : " + added_fb.size)
      val writer_fb = new PrintWriter(base_dir + "mentions_fb_added.txt", "utf-8")
      for(men <- added_fb)
        writer_fb.println(men)
      writer_fb.close()
      mens_addes ++= added_fb
    }

    //NL
    {
      val mens_join_nl = mens_in_two_kb & nl_mens
      val remained = nl_mens -- mens_join_nl
      val added_nl = mens_join_nl | remained.take((mens_join_nl.size * 0.2).toInt)
      println("nell added : " + added_nl.size)
      val writer_nl = new PrintWriter(base_dir + "mentions_nl_added.txt", "utf-8")
      for(men <- added_nl)
        writer_nl.println(men)
      writer_nl.close()
      mens_addes ++= added_nl
    }

    //RV
    {
      val mens_join_rv = mens_in_two_kb & rv_mens
      val remained = rv_mens -- mens_join_rv
      val added_rv = mens_join_rv | remained.take((mens_join_rv.size * 0.2).toInt)
      println("reverb added : " + added_rv.size)
      val writer_rv = new PrintWriter(base_dir + "mentions_rv_added.txt", "utf-8")
      for(men <- added_rv)
        writer_rv.println(men)
      writer_rv.close()
      mens_addes ++= added_rv
    }

    println("mention size with added kbs: " + mens_addes.size)
    val writer_added = new PrintWriter(base_dir + "mentions_added.txt", "utf-8")
    for(men <- mens_addes)
      writer_added.println(men)
    writer_added.close()
  }
}
