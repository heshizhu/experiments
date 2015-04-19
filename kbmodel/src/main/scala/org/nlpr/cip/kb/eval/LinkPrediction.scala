package org.nlpr.cip.kb.eval


import java.nio.file.{Paths, Files}

import org.nlpr.cip.kb.util.Corpus

import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer, Map, Set}
import scala.io.Source

/**
 * User: hesz
 * Date: 2014-12-26
 * Time: 16:27
 */
object LinkPrediction {
  val basePath = "G:/temp/TransX/"
//  val basePath = ""
  val encode = "utf-8"

  //实验参数
  var dim = 50
  var corpus = "fb15k"
  var model = "TransE"
  var negType = "bern"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())
  val (relation2type) = (Map[Int, String]())
  val (highFrqRelations) = Set[Int]()
  val (refRelations) = Set[Int]()


  def main(args: Array[String]) {
//    corpus = args(0)
//    model = args(1)
//    negType = args(2)
//    dim = args(3).toInt
    TransModel.normalize_flag = false


    corpus = "fb15k"
    model = "GEEL"
    negType = "bern"
    dim = 50

    println("corpus: " + corpus)
    println("model: " + model)
    println("negType: " + negType)
    println("dim: " + dim)

    prepare()

    val corpusAll = new Corpus(entity2id, relation2id, id2entity, id2relation)
    corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "train"))
    corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "valid"))
    corpusAll.add("%s%s/data/%s.txt".format(basePath, corpus, "test"))

    val linker = TransModel(basePath, corpus, model, negType, dim)
    println(linker)
    val testCorpus = new Corpus(entity2id, relation2id, id2entity, id2relation)
    testCorpus.add("%s%s/data/%s.txt".format(basePath, corpus, "test"))

    //总体评价
    val lpEval_Head_All = new LPEval()
    val lpEval_Tail_All = new LPEval()
    val lpEval_Relation_All = new LPEval()
    val lpEval_All = new LPEval()

    //按照关系进行评价
    val lpEval_Head_Relation = Map[Int, LPEval]()
    val lpEval_Tail_Relation = Map[Int, LPEval]()

    //按照关系类型进行评价
    val lpEval_Head_RelType = Map[String, LPEval]()
    val lpEval_Tail_RelType = Map[String, LPEval]()

    for ((rel, rel_type) <- relation2type) {
      lpEval_Head_Relation(rel) = new LPEval()
      lpEval_Tail_Relation(rel) = new LPEval()
      if (!lpEval_Head_RelType.contains(rel_type)) {
        lpEval_Head_RelType(rel_type) = new LPEval()
        lpEval_Tail_RelType(rel_type) = new LPEval()
      }
    }
    //按照高频低频关系进行评价
    val lpEval_Head_HLFrq = Map[Int, LPEval]()
    val lpEval_Tail_HLFrq = Map[Int, LPEval]()
    lpEval_Head_HLFrq(1) = new LPEval()
    lpEval_Head_HLFrq(0) = new LPEval()
    lpEval_Tail_HLFrq(1) = new LPEval()
    lpEval_Tail_HLFrq(0) = new LPEval()

    //按照是否自反关系进行评价
    val lpEval_Head_Ref = Map[Int, LPEval]()
    val lpEval_Tail_Ref = Map[Int, LPEval]()
    lpEval_Head_Ref(1) = new LPEval()
    lpEval_Head_Ref(0) = new LPEval()
    lpEval_Tail_Ref(1) = new LPEval()
    lpEval_Tail_Ref(0) = new LPEval()

    var count = 0
    for ((h, r, t) <- testCorpus.triples) {
      if (count % 100 == 0)
        println("id : " + count)
      count += 1

      //预测h
      val h_scores = Map[Int, Double]()
      for (neg_h <- corpusAll.relationHeads(r))
        h_scores(neg_h) = linker.distance(neg_h, r, t)
      val h_list = ListMap(h_scores.toSeq.sortWith(_._2 < _._2): _*).map(_._1).toList //从小到大排序
      val h_list_filt = for (tmp_h <- h_list if tmp_h == h || !corpusAll.exist(tmp_h, r, t)) yield tmp_h

      val index_raw_h = h_list.indexOf(h)
      assert(index_raw_h != -1)
      val index_filt_h = h_list_filt.indexOf(h)
      assert(index_filt_h != -1)
      val hits_raw_h = if (h_list.take(10).contains(h)) 1 else 0
      val hits_filt_h = if (h_list_filt.take(10).contains(h)) 1 else 0
      lpEval_Head_All.add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)
      lpEval_All.add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)

      //预测t
      val t_scores = Map[Int, Double]()
      for (neg_t <- corpusAll.relationTails(r))
        t_scores(neg_t) = linker.distance(h, r, neg_t)
      val t_list = ListMap(t_scores.toSeq.sortWith(_._2 < _._2): _*).map(_._1).toList
      val t_list_filt = for (tmp_t <- t_list if tmp_t == t || !corpusAll.exist(h, r, tmp_t)) yield tmp_t

      val index_raw_t = t_list.indexOf(t)
      assert(index_raw_t != -1)
      val index_filt_t = t_list_filt.indexOf(t)
      assert(index_filt_t != -1)
      val hits_raw_t = if (t_list.take(10).contains(t)) 1 else 0
      val hits_filt_t = if (t_list_filt.take(10).contains(t)) 1 else 0
      lpEval_Tail_All.add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)
      lpEval_All.add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)

      //分类型进行记录
      lpEval_Head_Relation(r).add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)
      lpEval_Tail_Relation(r).add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)

      //分类型进行记录-m-n
      val rel_type = relation2type(r)
      lpEval_Head_RelType(rel_type).add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)
      lpEval_Tail_RelType(rel_type).add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)

      //分类型进行记录-高频低频
      val r_hlfrq = if (highFrqRelations.contains(r)) 1 else 0 //1表示高频,0表示低频
      lpEval_Head_HLFrq(r_hlfrq).add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)
      lpEval_Tail_HLFrq(r_hlfrq).add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)

      val is_ref = if(refRelations.contains(r)) 1 else 0
      lpEval_Head_Ref(is_ref).add(index_raw_h, index_filt_h, hits_raw_h, hits_filt_h)
      lpEval_Tail_Ref(is_ref).add(index_raw_t, index_filt_t, hits_raw_t, hits_filt_t)

      //预测r
      val r_scores = Map[Int, Double]()
      for (neg_r <- corpusAll.allRelations)
        r_scores(neg_r) = linker.distance(h, neg_r, t)
      val r_list = ListMap(r_scores.toSeq.sortWith(_._2 < _._2): _*).map(_._1).toList
      val r_list_filt = for (tmp_r <- r_list if tmp_r == r || !corpusAll.exist(h, tmp_r, t)) yield tmp_r

      val index_raw_r = r_list.indexOf(r)
      assert(index_raw_r != -1)
      val index_filt_r = r_list_filt.indexOf(r)
      assert(index_filt_r != -1)
      val hits_raw_r = if (r_list.take(10).contains(r)) 1 else 0
      val hits_filt_r = if (r_list_filt.take(10).contains(r)) 1 else 0

      lpEval_Relation_All.add(index_raw_r, index_filt_r, hits_raw_r, hits_filt_r)
    }

    println("*****-----" + model + "-----*****")
    println("-------- 总体 --------")
    println("*****----- 全部 -----*****")
    println(lpEval_All.toSummary())
    println("*****----- Head -----*****")
    println(lpEval_Head_All.toSummary())
    println("*****----- Tail -----*****")
    println(lpEval_Tail_All.toSummary())
    println("*****----- Relation -----*****")
    println(lpEval_Relation_All.toSummary())
    println()
    println()

    println("-------- 高低频 --------")
    for (frq <- List(1, 0)) {
      val name = if (frq == 1) "高频" else "低频"
      println("-------- " + name + " --------")
      println("*****----- Head -----*****")
      println(lpEval_Head_HLFrq(frq).toSummary())
      println("*****----- Tail -----*****")
      println(lpEval_Tail_HLFrq(frq).toSummary())
      println()
    }
    println()
    println()

    println("-------- 自反关系 --------")
    for (ref <- List(1, 0)) {
      val name = if (ref == 1) "自反关系" else "非自反关系"
      println("-------- " + name + " --------")
      println("*****----- Head -----*****")
      println(lpEval_Head_Ref(ref).toSummary())
      println("*****----- Tail -----*****")
      println(lpEval_Tail_Ref(ref).toSummary())
      println()
    }
    println()
    println()


    println("-------- 关系类型 --------")
    for (rel_type <- List("1_1", "1_m", "m_1", "m_n")) {
      println("-------- " + rel_type + " --------")
      println("*****----- Head -----*****")
      println(lpEval_Head_RelType(rel_type).toSummary())
      println("*****----- Tail -----*****")
      println(lpEval_Tail_RelType(rel_type).toSummary())
      println()
    }
    println()
    println()

    println("-------- 关系 --------")
    for (rel <- relation2type.keys) {
      println("-------- " + rel + " --------")
      println("-------- " + id2relation(rel) + " --------")
      println("*****----- Head -----*****")
      println(lpEval_Head_Relation(rel).toSummary())
      println("*****----- Tail -----*****")
      println(lpEval_Tail_Relation(rel).toSummary())
      println()

    }
    println()
    println()
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
    println("load relation with relation type")
    for (line <- Source.fromFile(basePath + corpus + "/data/relation2reltype.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2type(terms(0).toInt) = terms(1)
    }
    println("load high frequent relation")
    for ((line, index) <- Source.fromFile(basePath + corpus + "/data/relationFrequent.txt", encode).getLines.zipWithIndex if index < 200) {
      val terms = line.split("\t")
      highFrqRelations += terms(0).toInt
    }
    println("load reflex relation")
    if(Files.exists(Paths.get(basePath + corpus + "/data/ref_relations.txt"))){
      for (line <- Source.fromFile(basePath + corpus + "/data/ref_relations.txt", encode).getLines) {
        val rel = line.split("\t")(0)
        refRelations += rel.toInt
      }
    }
  }
}


class LPEval {
  val meanRaw = ArrayBuffer[Int]()
  //排名
  val meanFilt = ArrayBuffer[Int]()
  val hits10Raw = ArrayBuffer[Int]()
  //前10个是否包含
  val hits10Filt = ArrayBuffer[Int]()

  def add(index_raw: Int, index_filt: Int, hits_raw: Int, hits_filt: Int) {
    meanRaw += index_raw
    meanFilt += index_filt
    hits10Raw += hits_raw
    hits10Filt += hits_filt
  }

  def toSummary(): String = {
    val v1 = if (meanRaw.size == 0) 0 * 1.0 else meanRaw.sum * 1.0 / meanRaw.size
    val v2 = if (meanFilt.size == 0) 0 * 1.0 else meanFilt.sum * 1.0 / meanFilt.size
    val v3 = if (hits10Raw.size == 0) 0 * 1.0 else hits10Raw.sum * 1.0 / hits10Raw.size
    val v4 = if (hits10Filt.size == 0) 0 * 1.0 else hits10Filt.sum * 1.0 / hits10Filt.size
    "\t\tmean\t\t\nraw: %f\tfilt: %f\n\t\thits@10\t\t\nraw: %f\tfilt: %f".format(v1, v2, v3, v4)
  }
}
