package org.nlpr.cip.kb.eval

import scala.io.Source
import collection.mutable.{Set, Map, ListBuffer}

object GuassEmbed {
  val basePath = "G:/temp/TransX/"
  val encode = "utf-8"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())
  val entity2types = Map[Int, Set[String]]()
  val relation2type = Map[Int, String]()
  val type2entities = Map[String, Set[Int]]()
  val triples = ListBuffer[(Int, Int, Int)]()
  val (rel2heads, rel2tails) = (Map[Int, Set[Int]](), Map[Int, Set[Int]]())

  var corpus = "fb15k"
  var model = "GEKL"
  var negType = "bern"
  var size = 50

  val ent2mean = Map[Int, Array[Double]]()
  val ent2vari = Map[Int, Array[Double]]()
  val rel2mean = Map[Int, Array[Double]]()
  val rel2vari = Map[Int, Array[Double]]()

  val ent2mentions = Map[Int, String]()

  def main(args: Array[String]) {
    prepare()
    println("prepare is over.")
    val path_two = "G:\\temp\\freebase\\entity_name_mini.txt"
    for(line <- Source.fromFile(path_two, "utf-8").getLines) {
      val terms = line.split("\t")
      val (ent, ment) = (terms(0), terms(1))
      if(entity2id.contains(ent)){
        ent2mentions(entity2id(ent)) = ment
      }
    }
    println("mention is over.")
    loadRepre()
    println("load representations is over.")

    //统计关系的密度与协方差的关系
        relDensityCov()
    //    relHeadTailDensityCov()
    //    headEntDensityCon()
    //    tailEntDensityCon()
    //    entDensityCon()

//        val ent = "/m/0fpzzp"
//        val ent_id = entity2id(ent)
//        val simEnts = similarEnt(ent_id)
//        println("find similar entity with : " + ent2mentions(ent_id))
//        for(simEnt <- simEnts.take(10))
//          println(ent2mentions(simEnt))

//    val rel = "/people/person/nationality"
//    val simRels = similarRel(relation2id(rel))
//    println("find similar relation with : " + rel)
//    for (simRel <- simRels.take(10))
//      println(id2relation(simRel))
  }

  //计算相似的关系
  def similarRel(rel: Int): List[Int] = {
    val rel_sim = Map[Int, Double]()
    for (id <- id2relation.keys if id != rel)
      rel_sim(id) = KL_Divergance(rel2mean(rel), rel2vari(rel), rel2mean(id), rel2vari(id))
    rel_sim.toSeq.sortWith(_._2 < _._2).toList.map(_._1)
  }

  //计算相似的实体
  def similarEnt(ent: Int): List[Int] = {
    val ent_sim = Map[Int, Double]()
    for (id <- id2entity.keys if id != ent)
      ent_sim(id) = KL_Divergance(ent2mean(ent), ent2vari(ent), ent2mean(id), ent2vari(id))
    ent_sim.toSeq.sortWith(_._2 < _._2).toList.map(_._1)
  }


  def entDensityCon() {
    val ent_triples = Map[Int, Int]()
    for ((h, r, t) <- triples) {
      ent_triples(h) = ent_triples.getOrElse(h, 0) + 1
      ent_triples(t) = ent_triples.getOrElse(t, 0) + 1
    }

    val ent_deter = Map[Int, Double]() //行列式
    val ent_trace = Map[Int, Double]() //迹

    for ((ent, vari) <- ent2vari.iterator) {
      ent_deter(ent) = determinant(ent2vari(ent))
      ent_trace(ent) = trace(ent2vari(ent))
    }

    val ent_sorted = ent_triples.toSeq.sortWith(_._2 > _._2)
    for ((ent, count) <- ent_sorted)
      println("%s\t%d\t%f\t%f".format(id2entity(ent), count, ent_deter(ent), ent_trace(ent)))
  }

  def tailEntDensityCon() {
    val ent_triples = Map[Int, Int]()
    for ((h, r, ent) <- triples)
      ent_triples(ent) = ent_triples.getOrElse(ent, 0) + 1

    val ent_deter = Map[Int, Double]() //行列式
    val ent_trace = Map[Int, Double]() //迹

    for ((ent, vari) <- ent2vari.iterator) {
      ent_deter(ent) = determinant(ent2vari(ent))
      ent_trace(ent) = trace(ent2vari(ent))
    }

    val ent_sorted = ent_triples.toSeq.sortWith(_._2 > _._2)
    for ((ent, count) <- ent_sorted)
      println("%s\t%d\t%f\t%f".format(id2entity(ent), count, ent_deter(ent), ent_trace(ent)))

  }

  def headEntDensityCon() {
    val ent_triples = Map[Int, Int]()
    for ((ent, r, t) <- triples)
      ent_triples(ent) = ent_triples.getOrElse(ent, 0) + 1

    val ent_deter = Map[Int, Double]() //行列式
    val ent_trace = Map[Int, Double]() //迹

    for ((ent, vari) <- ent2vari.iterator) {
      ent_deter(ent) = determinant(ent2vari(ent))
      ent_trace(ent) = trace(ent2vari(ent))
    }

    val ent_sorted = ent_triples.toSeq.sortWith(_._2 > _._2)
    for ((ent, count) <- ent_sorted)
      println("%s\t%d\t%f\t%f".format(id2entity(ent), count, ent_deter(ent), ent_trace(ent)))

  }

  def relDensityCov() {
    val rel_triples = Map[Int, Int]()
    for ((h, r, t) <- triples)
      rel_triples(r) = rel_triples.getOrElse(r, 0) + 1

    val rel_deter = Map[Int, Double]() //行列式
    val rel_trace = Map[Int, Double]() //迹

    for ((rel, vari) <- rel2vari.iterator) {
      rel_deter(rel) = determinant(rel2vari(rel))
      rel_trace(rel) = trace(rel2vari(rel))
    }

    val rel_sorted = rel_triples.toSeq.sortWith(_._2 > _._2)
    for ((rel, count) <- rel_sorted) {
      println("%s\t%d\t%d\t%d\t%s\t%f\t%f".format(
        id2relation(rel), count, rel2heads(rel).size, rel2tails(rel).size, relation2type(rel),
        rel_deter(rel), rel_trace(rel)))
    }
  }

  //前后所占比，与前后方差的关系
  def relHeadTailDensityCov() {
    val rel_triples = Map[Int, Int]()
    val rel_headsets = Map[Int, Set[Int]]()
    val rel_tailsets = Map[Int, Set[Int]]()
    for ((h, r, t) <- triples) {
      rel_triples(r) = rel_triples.getOrElse(r, 0) + 1
      val heads = rel_headsets.getOrElse(r, Set[Int]())
      heads.add(h)
      rel_headsets(r) = heads
      val tails = rel_tailsets.getOrElse(r, Set[Int]())
      tails.add(t)
      rel_tailsets(r) = tails
    }

    val rel_head_tail_density = Map[Int, Double]()
    for (r <- rel_headsets.keys)
      rel_head_tail_density(r) = rel_triples(r) * rel_headsets(r).size / (rel_headsets(r).size + rel_tailsets(r).size)

    val rel_deter = Map[Int, Double]() //行列式
    val rel_trace = Map[Int, Double]() //迹

    for ((rel, vari) <- rel2vari.iterator) {
      rel_deter(rel) = determinant(rel2vari(rel))
      rel_trace(rel) = trace(rel2vari(rel))
    }

    val rel_sorted = rel_head_tail_density.toSeq.sortWith(_._2 > _._2)
    for ((rel, density) <- rel_sorted) {
      println("%s\t%f\t%f\t%f".format(id2relation(rel), density, rel_deter(rel), rel_trace(rel)))
    }
  }

  def determinant(vari: Array[Double]): Double = vari.map(math.log(_)).sum

  def trace(vari: Array[Double]): Double = vari.sum

  //计算两个分布的KL距离
  def KL_Divergance(mean1: Array[Double], vari1: Array[Double],
                    mean2: Array[Double], vari2: Array[Double]): Double = {
    val d = mean1.length
    var score = 0.toDouble
    for (i <- 0 until d) {
      val mean = mean1(i) - mean2(i)
      score += (vari2(i) + mean * mean) / vari1(i)
      score -= math.log(vari2(i))
      score += math.log(vari1(i))
    }
    (score - d) / 2
  }


  def l2_distance(a: Array[Double], b: Array[Double]): Double =
    vecNorm_2(vecSub(a, b))

  def vecSub(a: Array[Double], b: Array[Double]): Array[Double] = (a zip b).map(x => x._1 - x._2)

  def vecNorm_2(a: Array[Double]): Double = math.sqrt(a.map(x => x * x).sum)

  def vecDot(a: Array[Double], b: Array[Double]): Double = (a zip b).map(x => x._1 * x._2).sum

  def vecCos(a: Array[Double], b: Array[Double]): Double = {
    val (a_n, b_n) = (vecNorm_2(a), vecNorm_2(b))
    if (a_n == 0 || b_n == 0) 0 else vecDot(a, b) / (a_n * b_n)
  }

  def loadRepre() {
    val entVecPath = "%s%s/%s/ent2gau.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0) {
      if (id % 2 == 0)
        ent2mean(id / 2) = line.split("\t").map(_.toDouble)
      else {
        val vari = line.split("\t").map(_.toDouble)
        ent2vari((id - 1) / 2) = vari
      }

    }
    val relVecPath = "%s%s/%s/rel2gau.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0) {
      if (id % 2 == 0)
        rel2mean(id / 2) = line.split("\t").map(_.toDouble)
      else {
        val vari = line.split("\t").map(_.toDouble)
        rel2vari((id - 1) / 2) = line.split("\t").map(_.toDouble)
      }

    }
  }

  def prepare() {
    for (line <- Source.fromFile(basePath + corpus + "/data/entity2id.txt", encode).getLines) {
      val terms = line.split("\t")
      entity2id(terms(0)) = terms(1).toInt
      id2entity(terms(1).toInt) = terms(0)
    }
    for (line <- Source.fromFile(basePath + corpus + "/data/relation2id.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2id(terms(0)) = terms(1).toInt
      id2relation(terms(1).toInt) = terms(0)
    }

    for (line <- Source.fromFile(basePath + corpus + "/data/relation2reltype.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2type(terms(0).toInt) = terms(1)
    }

    for (line <- Source.fromFile(basePath + corpus + "/data/train.txt", encode).getLines) {
      val terms = line.split("\t")
      triples += ((entity2id(terms(0)), relation2id(terms(1)), entity2id(terms(2))))

      val sub_id = entity2id(terms(0))
      val rel_id = relation2id(terms(1))
      val obj_id = entity2id(terms(2))

      val heads = rel2heads.getOrElse(rel_id, Set[Int]())
      heads.add(sub_id)
      rel2heads(rel_id) = heads

      val tails = rel2tails.getOrElse(rel_id, Set[Int]())
      tails.add(obj_id)
      rel2tails(rel_id) = tails
    }

    for (line <- Source.fromFile(basePath + corpus + "/data/entity2types.txt", encode).getLines) {
      val terms = line.split("\t")
      if (terms.length > 1) {
        val entID = entity2id(terms(0))
        for (i <- 1 until terms.length) {
          val entType = terms(i)

          val types = entity2types.getOrElse(entID, Set[String]())
          types.add(entType)
          entity2types(entID) = types

          val entis = type2entities.getOrElse(entType, Set[Int]())
          entis.add(entID)
          type2entities(entType) = entis
        }
      }
    }
  }
}
