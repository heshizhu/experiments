package org.nlpr.cip.kb.util

import java.io.PrintWriter

import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.io.Source
import scala.util.Random

class Corpus {
  val encode = "utf-8"
  val random = Random
  var negSampleType = "bern" //按照Bernoulli分布进行抽样

  val entity2id = Map[String, Int]()  //实体与ID对应表
  val relation2id = Map[String, Int]()  //关系与ID对应表
  val id2entity = Map[Int, String]()
  val id2relation = Map[Int, String]()

  val relHeads = Map[Int, Set[Int]]()  //关系与Head部分集合
  val relTails = Map[Int, Set[Int]]()  //关系与Tail部分集合

  val subRelObjs = Map[Int, Map[Int, Set[Int]]]()  // head relation tail

  val triples = ArrayBuffer[(Int, Int, Int)]()

  val tailNumPerHead = Map[Int, Double]() //平均一个head有多少个tails
  val headNumPerTail = Map[Int, Double]() //平均一个tail有多少个heads

  val allHeads = Set[Int]() //head集合
  val allRelations = Set[Int]() //relation集合
  val allTails = Set[Int]() //tail集合

  def this(ent2id: Map[String, Int], rel2id: Map[String, Int],
           id2ent: Map[Int, String], id2rel: Map[Int, String]) {
    this()
    entity2id ++= ent2id
    relation2id ++= rel2id
    id2entity ++= id2ent
    id2relation ++= id2rel
  }

  def this(filePath: String) {
    this()
    add(filePath)
  }

  def add(filePath: String) {
    for (line <- Source.fromFile(filePath, encode).getLines) {
      val Array(head, relation, tail) = line.split("\t")
      add(head, relation, tail)
    }
  }

  //添加一个三元组
  def add(head: String, relation: String, tail: String) {
    val headID = entityID(head)
    val tailID = entityID(tail)
    val relID = relationID(relation)

    if(!allHeads.contains(headID))
      allHeads += headID
    if(!allRelations.contains(relID))
      allRelations += relID
    if(!allTails.contains(tailID))
      allTails += tailID

    triples += ((headID, relID, tailID))

    val heads = relHeads.getOrElse(relID, Set[Int]())
    heads += headID
    relHeads(relID) = heads
    val tails = relTails.getOrElse(relID, Set[Int]())
    tails += tailID
    relTails(relID) = tails

    val relObjs = subRelObjs.getOrElse(headID, Map[Int, Set[Int]]())
    val objs = relObjs.getOrElse(relID, Set[Int]())
    objs += tailID
    relObjs(relID) = objs
    subRelObjs(headID) = relObjs
  }

  def entityNum = entity2id.size

  def relationNum = relation2id.size

  def relationHeads(relID: Int): Set[Int] = relHeads.getOrElse(relID, Set[Int]())

  def relationHeads(rel: String): Set[String] =
    if (!relation2id.contains(rel) || !relHeads.contains(relationID(rel))) Set[String]()
    else relHeads(relationID(rel)).map(id2entity(_))

  def relationTails(relID: Int): Set[Int] = relTails.getOrElse(relID, Set[Int]())

  def relationTails(rel: String): Set[String] =
    if (!relation2id.contains(rel) || !relTails.contains(relationID(rel))) Set[String]()
    else relTails(relationID(rel)).map(id2entity(_))

  def entity(entID: Int): String = id2entity(entID)

  def relation(relID: Int): String = id2relation(relID)

  def entityExist(ent: String) : Boolean = if (entity2id.contains(ent)) true else false
  def entityID(ent: String): Int = if (entity2id.contains(ent)) entity2id(ent)
  else {
    //println("%s missing in the entity set.".format(ent))
    val id: Int = if (entity2id.isEmpty) 0 else entity2id.values.max + 1
    entity2id(ent) = id; id2entity(id) = ent
    id
  }
  def relationExist(rel: String) : Boolean = if (relation2id.contains(rel)) true else false
  def relationID(rel: String): Int = if (relation2id.contains(rel)) relation2id(rel)
  else {
    //println("%s missing in the relation set.".format(rel))
    val id: Int = if (relation2id.isEmpty) 0 else relation2id.values.max + 1
    relation2id(rel) = id; id2relation(id) = rel
    id
  }

  def exist(h: Int, r: Int, t: Int): Boolean =
    if (!subRelObjs.contains(h) || !subRelObjs(h).contains(r) || !subRelObjs(h)(r).contains(t)) false
    else true

  def exist(h: String, r: String, t: String): Boolean =
    if (!entity2id.contains(h) || !entity2id.contains(t) || !relation2id.contains(r)) false
    else exist(entityID(h), relationID(r), entityID(r))


  //抽取一个负样本
  def negTriple(h: Int, r: Int, t: Int): (Int, Int, Int) = {
    val tempTriles = negTriples(h, r, t, 1)
    if(tempTriles.size > 0)
      tempTriles(0)
    else (-1, -1, -1)
  }

  //抽取多个负样本
  def negTriples(h: Int, r: Int, t: Int, num: Int): List[(Int, Int, Int)] = {
    val proChangeHead = if (negSampleType.eq("bern")) {
      val (tph, hpt) = getAvgH2T(r)
      tph * 1.0 / (tph + hpt)
    } else 0.5

    val negtriples = ArrayBuffer[(Int, Int, Int)]()
//    val (heads, tails) = (relationHeads(r).toList, relationTails(r).toList)
    val (heads, tails) = (allHeads.toList, allTails.toList)
    var whileIndex = 0 //循环次数
    while (negtriples.size < num && whileIndex < 10) {
      var (nh, nt) = (h, t)
      nh = heads(random.nextInt(heads.size))
//      if (random.nextFloat <= proChangeHead) //调换head
//        nh = heads(random.nextInt(heads.size))
//      else
//        nt = tails(random.nextInt(tails.size))
      if (!exist(nh, r, nt) && !negtriples.contains((nh, r, nt))){
        negtriples += ((nh, r, nt))
        whileIndex = 0
      }
      else
        whileIndex += 1
    }
    if(negtriples.size < num){
      var whileIndex = 0 //循环次数
      val (heads, tails) = (allHeads.toList, allTails.toList)
      while (negtriples.size < num && whileIndex < 10) {
        var (nh, nt) = (h, t)
        nh = heads(random.nextInt(heads.size))

//        if (random.nextFloat <= proChangeHead) //调换head
//          nh = heads(random.nextInt(heads.size))
//        else
//          nt = tails(random.nextInt(tails.size))
        if (!exist(nh, r, nt) && !negtriples.contains((nh, r, nt))){
          negtriples += ((nh, r, nt))
          whileIndex = 0
        }
        else
          whileIndex += 1
      }
      //println("全局搜索")
    }
    negtriples.toList
  }

  //对于指定关系，得到每个head对应tail个数的平均值和每个tail对应head个数的平均值
  def getAvgH2T(rel: String): (Double, Double) = getAvgH2T(relationID(rel))

  def getAvgH2T(relID: Int): (Double, Double) =
    if (tailNumPerHead.contains(relID) && headNumPerTail.contains(relID))
      (tailNumPerHead(relID), headNumPerTail(relID))
    else {
      val tailPerHead = Map[Int, Int]()
      val headPerTail = Map[Int, Int]()
      for ((h, r, t) <- triples if r == relID) {
        tailPerHead(h) = tailPerHead.getOrElse(h, 0) + 1
        headPerTail(t) = headPerTail.getOrElse(t, 0) + 1
      }
      tailNumPerHead(relID) = tailPerHead.values.sum * 1.0 / tailPerHead.size
      headNumPerTail(relID) = headPerTail.values.sum * 1.0 / headPerTail.size
      (tailNumPerHead(relID), headNumPerTail(relID))
    }
}

object Corpus {

  val basePath = "G:/temp/TransX/"
  val corpus = "fb15k_new"
  var encode = "utf-8"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())

  def main(args: Array[String]) {


//    generateIDMap()

    prepare()

//    generateBiIDMap()//产生双向IDmap
//    generateRelationType()


//    filter()//过滤掉没有出现在训练集中的实体和关系
//    filter_pos_neg()
    geneNegSamples()

    //filterTrain()

//    printHeadTail()
  }

  def printHeadTail(){
    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation) //包括训练集，验证集和测试集
    for (name <- List("train", "valid", "test")) {
      corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, name))
    }

    for(relID <- corpusAll.allRelations){
      val (tph, hpt) = corpusAll.getAvgH2T(relID)
      if(tph > 200 || hpt > 200)
      {
        println(corpusAll.relation(relID))
//        println("\t每个head平均有的tail个数：" + tph)
//        println("\t每个tail平均有的head个数：" + hpt)
      }
    }
  }

  //过滤没有在head出现过的实体
  def filterTrain(){

    val trainPath = basePath + corpus + "/data/train.txt"
    val entities = Source.fromFile(trainPath, encode).getLines.map(_.split("\t")(0)).toSet

    val out = new PrintWriter(basePath + corpus + "/data/train_filter.txt", encode)
    var count = 0
    for(line <- Source.fromFile(trainPath, encode).getLines) {
      val Array(sub, rel, obj) = line.split("\t")
      if(entities.contains(sub) && entities.contains(obj)) {
        count += 1
        out.println(line)
      }

    }
    out.close()

    println(entities.size)
    println(count)
  }




  def generateBiIDMap(){
    val neg_sign = "#-1"
    val trainPath = basePath + corpus + "/data/train.txt"
    //新产生的训练数据
    val newTrainOut = new PrintWriter(basePath + corpus + "/data/train_new.txt", encode)
    val entity2id = Map[String, Int]()
    val relation2id = Map[String, Int]()
    for(line <- Source.fromFile(trainPath, encode).getLines)
    {
      val Array(sub, rel, obj) = line.split("\t")
      if(!entity2id.contains(sub))
        entity2id(sub) = entity2id.size
      if(!entity2id.contains(obj))
        entity2id(obj) = entity2id.size
      if(!relation2id.contains(rel)){
        relation2id(rel) = relation2id.size
        relation2id(rel + neg_sign) = relation2id.size
      }
      newTrainOut.println("%s\t%s\t%s".format(sub, rel, obj))
      newTrainOut.println("%s\t%s\t%s".format(obj, (rel + neg_sign), sub))
    }
    newTrainOut.close()

    val ent2idOut = new PrintWriter(basePath + corpus + "/data/entity2id.txt", encode)
    for(ent <- entity2id.keys)
      ent2idOut.println(ent + "\t" + entity2id(ent))
    ent2idOut.close()

    val rel2idout = new PrintWriter(basePath + corpus + "/data/relation2id.txt", encode)
    for(rel <- relation2id.keys)
      rel2idout.println(rel + "\t" + relation2id(rel))
    rel2idout.close()
  }

  def generateRelationType(){
    val rel2reltype = Map[Int, Int]()
    val reltype2id = Map[String, Int]()
    for((relID, rel) <- id2relation.iterator) {
      val reltype = relationType(rel)
      if(!reltype2id.contains(reltype)){
        val reltypeID = reltype2id.size
        reltype2id(reltype) = reltypeID
      }
      val reltypeID = reltype2id(reltype)
      rel2reltype(relID) = reltypeID
    }

    val rel2reltypeout = new PrintWriter(basePath + corpus + "/data/relation2type.txt", encode)
    for((relID, reltypeID) <- rel2reltype.iterator)
      rel2reltypeout.println(relID + "\t" + reltypeID)
    rel2reltypeout.close()
  }

  def generateIDMap(){
    val trainPath = basePath + corpus + "/data/train.txt"
    val corpusTrain = new Corpus(trainPath)

    val ent2idOut = new PrintWriter(basePath + corpus + "/data/entity2id.txt", encode)
    for(entID <- 0 until corpusTrain.entityNum)
      ent2idOut.println(corpusTrain.entity(entID) + "\t" + entID)
    ent2idOut.close()

    val rel2idout = new PrintWriter(basePath + corpus + "/data/relation2id.txt", encode)
    for(relID <- 0 until corpusTrain.relationNum)
      rel2idout.println(corpusTrain.relation(relID) + "\t" + relID)
    rel2idout.close()

    for(name <- List("valid", "test")){
      val out = new PrintWriter(basePath + corpus + "/data/" + name + "_filter.txt")
      for(line <- Source.fromFile(basePath + corpus + "/data/" + name + ".txt").getLines){
        val Array(sub, rel, obj) = line.split("\t")
        if(corpusTrain.entity2id.contains(sub)
          && corpusTrain.entity2id.contains(obj) && corpusTrain.relation2id.contains(rel))
          out.println(line)
      }
      out.close()
    }


  }

  def relationType(rel:String) : String =
    if(rel.split("/").length > 2) rel.split("/").take(3).mkString("/") else rel

  def filter(){
    for(name <- List("valid", "test")) {
      val output = new PrintWriter(basePath + corpus + "/data/" + name + "_filter.txt", encode)
      var count = 0
      for(line <- Source.fromFile(basePath + corpus + "/data/" + name + ".txt" , encode).getLines) {
        val Array(sub, rel, obj) = line.split("\t")
        if (entity2id.contains(sub) && entity2id.contains(obj) && relation2id.contains(rel)) {
          output.println(line)
        }
        else {
//          println("filter: " + line)
          count += 1
        }
      }
      println(name + ": " + count)

      output.close()
    }
  }

  def filter_pos_neg(){
    for(negT <- List("bern")){
      for(name <- List("valid", "test")){
        val outPos = new PrintWriter(basePath + corpus + "/data/" + name + "_pos_" + negT + ".txt", encode)
        val outNeg = new PrintWriter(basePath + corpus + "/data/" + name + "_neg_" + negT + ".txt", encode)
        for(line <- Source.fromFile(basePath + corpus + "/data/" + name + ".txt" , encode).getLines){
          val terms = line.split("\t")
          val (sub, rel, obj) = (terms(0), terms(1), terms(2))
          if(entity2id.contains(sub) && entity2id.contains(obj) && relation2id.contains(rel)){
            if(terms.length == 4){
              if(terms(3) == "1")
                outPos.println(sub + "\t" + rel + "\t" + obj)
              else
                outNeg.println(sub + "\t" + rel + "\t" + obj)
            }
            else
              outPos.println(sub + "\t" + rel + "\t" + obj)
          }
          else{
            println("不存在: " + sub + "\t" + rel + "\t" + obj)
          }
        }
        outPos.close()
        outNeg.close()
      }
    }
  }

  def prepare(){
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

  def geneNegSamples() {
    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation) //包括训练集，验证集和测试集
    for (name <- List("train")) {
      corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, name))
    }

    for(negst <- List("bern", "unif")){

      corpusAll.negSampleType = negst

      for (name <- List("valid", "test")) {
        val corpusNew = new Corpus(entity2id, relation2id, id2entity, id2relation)
        corpusNew.add("%s%s/data/%s.txt".format(basePath, corpus, name))

        val outPos = new PrintWriter("%s%s/data/%s_pos_%s.txt".format(basePath, corpus, name, corpusAll.negSampleType))
        val outNeg = new PrintWriter("%s%s/data/%s_neg_%s.txt".format(basePath, corpus, name, corpusAll.negSampleType))
        for (((h, r, t), ind) <- corpusNew.triples.toList.zipWithIndex) {
          val (nh, nr, nt) = corpusAll.negTriple(h, r, t)
          if(nh != -1){
            outPos.println("%s\t%s\t%s".format(
              corpusAll.entity(h), corpusAll.relation(r), corpusAll.entity(t)))
            outNeg.println("%s\t%s\t%s".format(
              corpusAll.entity(nh), corpusAll.relation(nr), corpusAll.entity(nt)))
          }
          else{
            println("过滤掉：" + ind)
          }
        }
        outPos.close()
        outNeg.close()
      }
    }
  }
}
