package org.nlpr.cip.kb.util

import scala.collection.mutable.{Map, Set, ArrayBuffer}
import scala.io.Source

/**
 * User: hesz
 * Date: 2015-1-7
 * Time: 16:37
 */
object CorpusStat {

  val basePath = "G:/temp/TransX/"
  val corpus = "fb15k_filter"
  var encode = "utf-8"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())
  val rel2relType = Map[Int, Int]()
  val (id2relType, relType2id) = (Map[Int, String](), Map[String, Int]())

  def main_2(args: Array[String]) {
    prepare()

    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation) //包括训练集，验证集和测试集
    for (name <- List("train", "valid", "test"))
      corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, name))

    //计算一个实体平均有多少个关系类型，每个类型有多少个关系
    val ent2relType = Map[Int, Set[Int]]()
    val relType2rel = Map[Int, Set[Int]]()
    for((h, r, t) <- corpusAll.triples){
      val relID = r
      val relTypeID = rel2relType(r)

      val relTypes = ent2relType.getOrElse(h, Set[Int]())
      relTypes += relTypeID
      ent2relType(h) = relTypes

      val rels = relType2rel.getOrElse(relTypeID, Set[Int]())
      rels += relID
      relType2rel(relTypeID) = rels
    }

    val v1 = ent2relType.size //实体个数
    val v2 = ent2relType.map(_._2.size).sum * 1.0 / v1
    val v3 = ent2relType.map(_._2.size).max
    val v4 = ent2relType.map(_._2.size).min
    println(v1 + "\t" + v2 + "\t" + v3 + "\t" + v4)

    val v5 = relType2rel.size //关系类型个数
    val v6 = relType2rel.map(_._2.size).sum * 1.0 / v5
    val v7 = relType2rel.map(_._2.size).max
    val v8 = relType2rel.map(_._2.size).min
    println(v5 + "\t" + v6 + "\t" + v7 + "\t" + v8)

  }

  def relationType(rel:String) : String =
    if(rel.split("/").length > 2) rel.split("/").take(3).mkString("/") else rel

  //计算平均入度，出度等
  def main(args: Array[String]) {
    prepare()

    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation) //包括训练集，验证集和测试集
    val countBuffer = ArrayBuffer[Int]()
    for (name <- List("train", "valid", "test")) {
      corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, name))
      countBuffer += corpusAll.triples.size
    }

    print(corpusAll.relationNum + "\t" + corpusAll.entityNum + "\t")
    val n1 = countBuffer(0)
    val n2 = countBuffer(1) - countBuffer(0)
    val n3 = countBuffer(2) - countBuffer(1)
    print(n1 + "\t" + n2 + "\t" + n3 + "\t")

    val forwRelCount = Map[Int, Set[Int]]()//key为实体，value为该实体拥有的正向关系个数
    val backRelCount = Map[Int, Set[Int]]()//key为实体，value为该实体拥有的反向关系个数

    val forwTriCount = Map[Int, Int]()//key为实体，value为该实体拥有的正向三元组个数
    val backTriCount = Map[Int, Int]()//key为实体，value为该实体拥有的反向三元组个数

    for((h,r,t) <- corpusAll.triples){
      val frc = forwRelCount.getOrElse(h, Set[Int]())
      frc += r
      forwRelCount(h) = frc
      val brc = backRelCount.getOrElse(t, Set[Int]())
      brc += r
      backRelCount(t) = brc
      forwTriCount(h) = forwTriCount.getOrElse(h, 0) + 1
      backTriCount(t) = backTriCount.getOrElse(t, 0) + 1
    }

    val v1 = forwRelCount.map(_._2.size).sum * 1.0 / forwRelCount.size
    val v1Max = forwRelCount.map(_._2.size).max
    val v1Min = forwRelCount.map(_._2.size).min

    val v2 = backRelCount.map(_._2.size).sum * 1.0 / backRelCount.size
    val v2Max = backRelCount.map(_._2.size).max
    val v2Min = backRelCount.map(_._2.size).min

    val v3 = forwTriCount.values.sum * 1.0 / forwTriCount.size
    val v3Max = forwTriCount.values.max
    val v3Min = forwTriCount.values.min

    val v4 = backTriCount.values.sum * 1.0 / backTriCount.size

    val v4Max = backTriCount.values.max
    val v4Min = backTriCount.values.min
    println(v1 + "\t" + v2 + "\t" + v3 + "\t" + v4)
    println("[" + v1Min + "," + v1Max + "]")
    println("[" + v2Min + "," + v2Max + "]")
    println("[" + v3Min + "," + v3Max + "]")
    println("[" + v4Min + "," + v4Max + "]")
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
      val relType = relationType((terms(0)))

      if(!relType2id.contains(relType)){
        val relTypeID = id2relType.size
        id2relType(relTypeID) = relType
        relType2id(relType) = relTypeID
      }
      val relTypeID = relType2id(relType)
      rel2relType(terms(1).toInt) = relTypeID
    }
  }
}
