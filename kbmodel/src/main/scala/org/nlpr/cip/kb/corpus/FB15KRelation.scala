package org.nlpr.cip.kb.corpus

import java.io.PrintWriter


import org.nlpr.cip.kb.util.Corpus

import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.io.Source

/**
 * 1. 判断1-m, m-1, 1-1, m-n四种类型关系
 * 2. 关系-三元组频率
 */
object FB15KRelation {
  val basePath = "G:/temp/TransX/"
  val encode = "utf-8"
  var corpus = "fb15k_filter_bi"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())


  def main(args: Array[String]) {
    prepare()
    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation)
    corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "train"))
    //corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "valid_pos_unif"))
    //corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "test_pos_unif"))

    //计算高频与低频关系
    val relCount = Map[Int, Int]()
    for ((h, r, t) <- corpusAll.triples)
      relCount(r) = if (relCount.contains(r)) relCount(r) + 1 else 1
    val sortedSeq = relCount.toSeq.sortWith(_._2 > _._2)
    val relCountPath = "%s%s/data/%s.txt".format(basePath, corpus, "relationFrequent")
    val relCountOut = new PrintWriter(relCountPath)
    for ((k, v) <- sortedSeq)
      relCountOut.println(k + "\t" + v)
    relCountOut.close()
    val sortedRels = sortedSeq.map(_._1)
    val sortedCount = sortedSeq.map(_._2)
    println(200.0 / sortedCount.size)
    println(sortedCount.take(200).sum * 1.0 / sortedCount.sum) //前200个关系，占14.87%的关系，占88%的三元组

    //统计1-m, m-1, m-n, 1-1关系
    val subRelObj = Map[Int, Map[Int, Set[Int]]]()
    val objRelSub = Map[Int, Map[Int, Set[Int]]]()
    for ((h, r, t) <- corpusAll.triples) {
      val relobjs = subRelObj.getOrElse(h, Map[Int, Set[Int]]())
      val objs = relobjs.getOrElse(r, Set[Int]())
      objs += t
      relobjs(r) = objs
      subRelObj(h) = relobjs

      val relsubs = objRelSub.getOrElse(t, Map[Int, Set[Int]]())
      val subs = relsubs.getOrElse(r, Set[Int]())
      subs += h
      relsubs(r) = subs
      objRelSub(t) = relsubs
    }

    val tailsPerHeadSet = Map[Int, ArrayBuffer[Int]]()
    val headsPerTailSet = Map[Int, ArrayBuffer[Int]]()
    for (v <- subRelObj.values)
      for (r <- v.keys) {
        val tph = tailsPerHeadSet.getOrElse(r, ArrayBuffer[Int]())
        tph += v(r).size
        tailsPerHeadSet(r) = tph
      }
    for (v <- objRelSub.values)
      for (r <- v.keys) {
        val hpt = headsPerTailSet.getOrElse(r, ArrayBuffer[Int]())
        hpt += v(r).size
        headsPerTailSet(r) = hpt
      }
    val tailPerHead = Map[Int, Double]()
    val headPerTail = Map[Int, Double]()
    for ((k, v) <- tailsPerHeadSet)
      tailPerHead(k) = v.sum * 1.0 / v.size
    for ((k, v) <- headsPerTailSet)
      headPerTail(k) = v.sum * 1.0 / v.size

    var num11 = 0
    var num1m = 0
    var numm1 = 0
    var nummn = 0
    val rel_type_path = "%s%s/data/%s.txt".format(basePath, corpus, "relation2reltype")
    val rel_type_out = new PrintWriter(rel_type_path)


    for (r <- tailPerHead.keys) {
      if (tailPerHead(r) < 1.5 && headPerTail(r) < 1.5) {
        num11 += 1
        rel_type_out.println(r + "\t1_1")
      }
      else if (tailPerHead(r) < 1.5 && headPerTail(r) >= 1.5) {
        numm1 += 1
        rel_type_out.println(r + "\tm_1")
      }
      else if (tailPerHead(r) >= 1.5 && headPerTail(r) < 1.5) {
        num1m += 1
        rel_type_out.println(r + "\t1_m")
      }
      else {
        nummn += 1
        rel_type_out.println(r + "\tm_n")
      }
    }

    rel_type_out.close()

    println(tailPerHead.size)
    println(num11 + "\t" + (num11 * 1.0 / tailPerHead.size))
    println(num1m + "\t" + (num1m * 1.0 / tailPerHead.size))
    println(numm1 + "\t" + (numm1 * 1.0 / tailPerHead.size))
    println(nummn + "\t" + (nummn * 1.0 / tailPerHead.size))
  }

  def prepare() {
    println("load entity with id")
    for (line <- Source.fromFile(basePath + corpus + "/data/entity2id.txt", encode).getLines) {
      val terms = line.split("\t")
      entity2id(terms(0)) = terms(1).toInt
      id2entity(terms(1).toInt) = terms(0)
    }
    println("load relation with id")
    for (line <- Source.fromFile(basePath + corpus + "/data/relation2id.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2id(terms(0)) = terms(1).toInt
      id2relation(terms(1).toInt) = terms(0)
    }
  }


  val rel_domains = Map[String, String]()
  val rel_ranges = Map[String, String]()

  def main_2(args: Array[String]) {



    loadRelDomainRange()

    val rel_id_path = "G:\\temp\\TransX\\fb15k_filter\\data\\relation2id.txt"
    val relations = Source.fromFile(rel_id_path, "utf-8").getLines().map(_.split("\t")(0)).toList

    val output_path = "G:\\temp\\TransX\\fb15k_filter\\data\\relation2domainrange.txt"
    val writer = new PrintWriter(output_path, "utf-8")

    for (rel <- relations) {
      var index = 0
      if (rel.lastIndexOf(".") != -1)
        index = rel.lastIndexOf(".") + 1
      val rel_filter = rel.substring(index + 1).split("/").mkString(".")
      val domain = relationType(rel).substring(1).split("/").mkString(".")
      var range = rel_ranges.getOrElse(rel_filter, "null")
      writer.println("%s\t%s\t%s".format(rel, domain, range))

    }
    writer.close()
  }

  def relationType(rel:String) : String =
    if(rel.split("/").length > 2) rel.split("/").take(3).mkString("/") else rel

  def loadRelDomainRange() {
    val dataPath = "G:\\datasets\\Freebase\\fbpredicates-all.txt"

    for (line <- Source.fromFile(dataPath, "utf-8").getLines()) {
      val terms = line.split("\t")
      if (terms(1).equals("<http://rdf.freebase.com/ns/type.property.schema>")) {
        val sub = filter(terms(0))
        val domain = filter(terms(2))
        rel_domains(sub) = domain
      }
      else if (terms(1).equals("<http://rdf.freebase.com/ns/type.property.expected_type>")) {
        val sub = filter(terms(0))
        val range = filter(terms(2))
        rel_ranges(sub) = range
      }
    }
    println(rel_domains.size)
    println(rel_ranges.size)
  }

  def filter(uri: String): String =
    if (uri.startsWith("<http://rdf.freebase.com/ns/") && uri.endsWith(">"))
      uri.substring("<http://rdf.freebase.com/ns/".length, uri.length - 1)
    else uri

}
