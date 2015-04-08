package org.nlpr.cip.kb.eval

import scala.io.Source
import collection.mutable.{Set, Map}

object GuassEmbed {
  val basePath = "G:/temp/TransX/"
  val encode = "utf-8"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())
  val entity2types = Map[Int, Set[String]]()
  val type2entities = Map[String, Set[Int]]()

  var corpus = "fb15k"
  var model = "GEKL"
  var negType = "unif"
  var size = 50

  val ent2mean = Map[Int, Array[Double]]()
  val ent2vari = Map[Int, Array[Double]]()
  val rel2mean = Map[Int, Array[Double]]()
  val rel2vari = Map[Int, Array[Double]]()

  def main(args: Array[String]) {

    prepare()
    println("prepare is over.")
    loadRepre()
    println("load is over.")

    printDensity("business.employer")
  }

  //打印类别的密度：输出其类内节点的平均余弦和内间节点的平均余弦
  def printDensity(entType: String){
    println("type[" + entType + "]")

    val entityIn = type2entities(entType).toList.take(10)
    val entInTypes = Set[String]()
    for(ent <- entityIn)
      for(temp_type <- entity2types(ent))
        entInTypes.add(temp_type)

    val entityOutTemp = Set[Int]()
      for(ent <- id2entity.keys if entity2types.contains(ent)){
      val xxx = entity2types(ent) & entInTypes
      if(xxx.size == 0)
        entityOutTemp.add(ent)
    }
    val entityOut = entityOutTemp.toList.take(10)

    var sumIn:Double = 0
    var countIn = 0

    for(i <- 0 until entityIn.size; j <- i + 1 until entityIn.size){
      sumIn += l2_distance(ent2vari(entityIn(i)), ent2vari(entityIn(j)))
      countIn += 1
    }
    println("density [In] : " + (sumIn / countIn))

    var sumOut:Double = 0
    var countOut = 0
    for(i <- 0 until entityIn.size; j <- 0 until entityOut.size){
      sumOut += l2_distance(ent2vari(entityIn(i)),ent2vari(entityOut(j)))
      countOut += 1
    }
    println("density[Out] : " + (sumOut / countOut))
    println()
  }

  def l2_distance(a: Array[Double], b: Array[Double]): Double =
    vecNorm_2(vecSub(a, b))
  def vecSub(a: Array[Double], b: Array[Double]): Array[Double] = (a zip b).map(x => x._1 - x._2)
  def vecNorm_2(a: Array[Double]): Double = math.sqrt(a.map(x => x * x).sum)
  def vecDot(a: Array[Double], b: Array[Double]): Double = (a zip b).map(x => x._1 * x._2).sum
  def vecCos(a:Array[Double], b:Array[Double]): Double = {
    val (a_n, b_n) = (vecNorm_2(a), vecNorm_2(b))
    if(a_n == 0 || b_n == 0) 0 else vecDot(a, b) / (a_n * b_n)
  }

  def loadRepre(){
    val entVecPath = "%s%s/%s/ent2gau.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0){
      if(id % 2 == 0)
        ent2mean(id / 2) = line.split("\t").map(_.toDouble)
      else
        ent2vari((id - 1) / 2) = line.split("\t").map(_.toDouble)
    }
    val relVecPath = "%s%s/%s/rel2gau.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0){
      if(id % 2 == 0)
        rel2mean(id / 2) = line.split("\t").map(_.toDouble)
      else
        rel2vari((id - 1) / 2) = line.split("\t").map(_.toDouble)
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
