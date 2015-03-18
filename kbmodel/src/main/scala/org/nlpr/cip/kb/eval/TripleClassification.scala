package org.nlpr.cip.kb.eval

import org.nlpr.cip.kb.util.Corpus

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map}

object TripleClassification {
  val encode = "utf-8"

  var corpus = "fb15k"
  var model = "TransE"
  var negType = "bern"
  var dimNum = 30

  val basePath = "G:/temp/TransX/"

  val (entity2id, relation2id) = (Map[String, Int](), Map[String, Int]())
  val (id2entity, id2relation) = (Map[Int, String](), Map[Int, String]())
  val relation2type = Map[Int, String]()
  var highFrqRelations = Set[Int]()

  def eval(v1: String = "fb15k_new",
           v2: String = "TransJR_L1",
           v3: String = "bern",
           v4: Int = 50): Double = {
    corpus = v1
    model = v2
    negType = v3
    dimNum = v4
    prepare()

//    TransModel.normalize_flag = true
//    println("corpus: " + corpus)
//    println("model: " + model)
//    println("类型：" + negType)
//    println("向量维度：" + dimNum)
//    validtest()


    TransModel.normalize_flag = false
    println("corpus: " + corpus)
    println("model: " + model)
    println("类型：" + negType)
    println("向量维度：" + dimNum)
    validtest()
  }

  def main(args: Array[String]) {
//    prepare()

    corpus = "fb15k_new"
    model = "TransH_L1"

    for (size <- List(50)) {
      for (negT <- List("bern", "unif")) {
        dimNum = size
        negType = negT
        eval(corpus, model, negT, size)
      }
    }
  }

  def validtest(): Double = {
    val tc: TransModel = TransModel(basePath, corpus, model, negType, dimNum)
//    println("init tc model: " + tc)

    //验证集上每个三元组得分
    val validPosCorpus = new Corpus(entity2id, relation2id, id2entity, id2relation)
    validPosCorpus.add("%s%s/data/%s_pos_%s.txt".format(basePath, corpus, "valid", negType))
    val validNegCorpus = new Corpus(entity2id, relation2id, id2entity, id2relation)
    validNegCorpus.add("%s%s/data/%s_neg_%s.txt".format(basePath, corpus, "valid", negType))


    val s1 = validPosCorpus.triples.map(_._2).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    val s2 = validNegCorpus.triples.map(_._2).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    assert((s1.toSet -- s2.toSet).size == 0)
    assert((s2.toSet -- s1.toSet).size == 0)


    val validPosScores = ArrayBuffer[(Int, Double)]()
    val validNegScores = ArrayBuffer[(Int, Double)]()
    for ((h, r, t) <- validPosCorpus.triples)
      validPosScores += ((r, tc.distance(h, r, t)))
    for ((h, r, t) <- validNegCorpus.triples)
      validNegScores += ((r, tc.distance(h, r, t)))

    val s3 = validPosScores.map(_._1).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    val s4 = validNegScores.map(_._1).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    assert((s3.toSet -- s4.toSet).size == 0)
    assert((s4.toSet -- s3.toSet).size == 0)


    //测试集上每个三元组得分
    val testPosCorpus = new Corpus(entity2id, relation2id, id2entity, id2relation)
    testPosCorpus.add("%s%s/data/%s_pos_%s.txt".format(basePath, corpus, "test", negType))
    val testNegCorpus = new Corpus(entity2id, relation2id, id2entity, id2relation)
    testNegCorpus.add("%s%s/data/%s_neg_%s.txt".format(basePath, corpus, "test", negType))

    val s5 = testPosCorpus.triples.map(_._2).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    val s6 = testNegCorpus.triples.map(_._2).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    assert((s5.toSet -- s6.toSet).size == 0)
    assert((s6.toSet -- s5.toSet).size == 0)

    val testPosScores = ArrayBuffer[(Int, Double)]()
    val testNegScores = ArrayBuffer[(Int, Double)]()
    for ((h, r, t) <- testPosCorpus.triples)
      testPosScores += ((r, tc.distance(h, r, t)))
    for ((h, r, t) <- testNegCorpus.triples)
      testNegScores += ((r, tc.distance(h, r, t)))

    val s7 = validPosScores.map(_._1).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    val s8 = validNegScores.map(_._1).toSet.toSeq.sortWith((x: Int, y: Int) => x > y)
    assert((s7.toSet -- s8.toSet).size == 0)
    assert((s8.toSet -- s7.toSet).size == 0)
    //统计阈值，计算效果

    //全部统计
    {
//      println("全部效果")
      val validPosValues = validPosScores.toList
      val validNegValues = validNegScores.toList
      val testPosValues = testPosScores.toList
      val testNegValues = testNegScores.toList
      validtest(validPosValues, validNegValues, testPosValues, testNegValues)
    }

    //测试低频关系
    //    {
    //      println("高频关系效果")
    //      def filterFun = (x:(Int,Double)) => (if(highFrqRelations.contains(x._1)) true else false)
    //      val validPosValues = validPosScores.filter(filterFun).toList
    //      val validNegValues = validNegScores.filter(filterFun).toList
    //      val testPosValues = testPosScores.filter(filterFun).toList
    //      val testNegValues = testNegScores.filter(filterFun).toList
    //      validtest(validPosValues, validNegValues,testPosValues, testNegValues)
    //    }

    //测试低频关系
    //    {
    //      println("低频关系效果")
    //      def filterFun = (x:(Int,Double)) => (if(!highFrqRelations.contains(x._1)) true else false)
    //      val validPosValues = validPosScores.filter(filterFun).toList
    //      val validNegValues = validNegScores.filter(filterFun).toList
    //      val testPosValues = testPosScores.filter(filterFun).toList
    //      val testNegValues = testNegScores.filter(filterFun).toList
    //      validtest(validPosValues, validNegValues, testPosValues, testNegValues)
    //    }

    //1_1
    //    {
    //      println("1_1")
    //      val validPosValues = (for(((_,r,_), s) <- validPosScores if relation2type(r) == "1_1") yield s).toList
    //      val validNegValues = (for(((_,r,_), s) <- validNegScores if relation2type(r) == "1_1") yield s).toList
    //      val testPosValues = (for(((_,r,_), s) <- testPosScores if relation2type(r) == "1_1") yield s).toList
    //      val testNegValues = (for(((_,r,_), s) <- testNegScores if relation2type(r) == "1_1") yield s).toList
    //      validtest(validPosValues.toSeq.map(transform), validNegValues.toSeq.map(transform),
    //        testPosValues.toSeq.map(transform), testNegValues.toSeq.map(transform))
    //    }

    //1_m
    //    {
    //      println("1_m")
    //      val validPosValues = (for(((_,r,_), s) <- validPosScores if relation2type(r) == "1_m") yield s).toList
    //      val validNegValues = (for(((_,r,_), s) <- validNegScores if relation2type(r) == "1_m") yield s).toList
    //      val testPosValues = (for(((_,r,_), s) <- testPosScores if relation2type(r) == "1_m") yield s).toList
    //      val testNegValues = (for(((_,r,_), s) <- testNegScores if relation2type(r) == "1_m") yield s).toList
    //      validtest(validPosValues, validNegValues, testPosValues, testNegValues)
    //    }

    //m_1
    //    {
    //      println("m_1")
    //      val validPosValues = (for(((_,r,_), s) <- validPosScores if relation2type(r) == "m_1") yield s).toList
    //      val validNegValues = (for(((_,r,_), s) <- validNegScores if relation2type(r) == "m_1") yield s).toList
    //      val testPosValues = (for(((_,r,_), s) <- testPosScores if relation2type(r) == "m_1") yield s).toList
    //      val testNegValues = (for(((_,r,_), s) <- testNegScores if relation2type(r) == "m_1") yield s).toList
    //      validtest(validPosValues.toSeq.map(transform), validNegValues.toSeq.map(transform),
    //        testPosValues.toSeq.map(transform), testNegValues.toSeq.map(transform))
    //    }

    //m_n
    //    {
    //      println("m_n")
    //      val validPosValues = (for(((_,r,_), s) <- validPosScores if relation2type(r) == "m_n") yield s).toList
    //      val validNegValues = (for(((_,r,_), s) <- validNegScores if relation2type(r) == "m_n") yield s).toList
    //      val testPosValues = (for(((_,r,_), s) <- testPosScores if relation2type(r) == "m_n") yield s).toList
    //      val testNegValues = (for(((_,r,_), s) <- testNegScores if relation2type(r) == "m_n") yield s).toList
    //      validtest(validPosValues.toSeq.map(transform), validNegValues.toSeq.map(transform),
    //        testPosValues.toSeq.map(transform), testNegValues.toSeq.map(transform))
    //    }

    //    {
    //      //每种关系一个阈值
    //      for(relID <- relation2id.values){
    //        println("关系：" + id2relation(relID))
    //        val validPosValues = (for(((_,r,_), s) <- validPosScores if r == relID) yield s).toList
    //        val validNegValues = (for(((_,r,_), s) <- validNegScores if r == relID) yield s).toList
    //        val testPosValues = (for(((_,r,_), s) <- testPosScores if r == relID) yield s).toList
    //        val testNegValues = (for(((_,r,_), s) <- testNegScores if r == relID) yield s).toList
    //        validtest(validPosValues.toSeq.map(transform), validNegValues.toSeq.map(transform),
    //          testPosValues.toSeq.map(transform), testNegValues.toSeq.map(transform))
    //      }
    //    }
  }

  //总共一个阈值
  def validtest_all(validPosValues: List[(Int, Double)], validNegValues: List[(Int, Double)],
                    testPosValues: List[(Int, Double)], testNegValues: List[(Int, Double)]) {
    if (validPosValues.size == 0 || validNegValues.size == 0
      || testPosValues.size == 0 || testNegValues.size == 0)
      println("没有得分")
    else {
      //首先得到总体阈值
      var thresholdAll: Double = 0 //全部的阈值
      var scoreAll = 0
      val validPosValuesAll = validPosValues.map(_._2).toList
      val validNegValuesAll = validNegValues.map(_._2).toList
      val min = (List(validPosValuesAll.min, validNegValuesAll.min).min * 100).toInt
      val max = (List(validPosValuesAll.max, validNegValuesAll.max).max * 100).toInt
      for (lambda <- min.to(max); threshold = lambda * 0.01) {
        val score = validPosValuesAll.count(_ < threshold) + validNegValuesAll.count(_ >= threshold)
        if (score > scoreAll) {
          scoreAll = score
          thresholdAll = threshold
        }
      }
      println("[验证集]正确率为: " + scoreAll * 1.0 / (validPosValues.size + validNegValues.size))
      val testPosValuesAll = testPosValues.map(_._2).toList
      val testNegValuesAll = testNegValues.map(_._2).toList
      val truePos = testPosValuesAll.count(_ < thresholdAll)
      val trueNeg = testNegValuesAll.count(_ >= thresholdAll)
      val testFinal = (truePos + trueNeg) * 1.0 / (testPosValues.size + testNegValues.size)
      println("[测试集]正确率为: " + testFinal)
    }
  }

  //每种关系一个阈值
  def validtest(validPosValues: List[(Int, Double)], validNegValues: List[(Int, Double)],
                testPosValues: List[(Int, Double)], testNegValues: List[(Int, Double)]): Double = {
    if (validPosValues.size == 0 || validNegValues.size == 0
      || testPosValues.size == 0 || testNegValues.size == 0) {
      println("没有得分")
      return 0
    }
    else {
      //首先得到总体阈值
      var thresholdAll: Double = 0 //全部的阈值
      var scoreAll = 0
      val validPosValuesAll = validPosValues.map(_._2).toList
      val validNegValuesAll = validNegValues.map(_._2).toList
      val min = (List(validPosValuesAll.min, validNegValuesAll.min).min * 100).toInt
      val max = (List(validPosValuesAll.max, validNegValuesAll.max).max * 100).toInt
      for (lambda <- min.to(max); threshold = lambda * 0.01) {
        val score = validPosValuesAll.count(_ < threshold) + validNegValuesAll.count(_ >= threshold)
        if (score > scoreAll) {
          scoreAll = score
          thresholdAll = threshold
        }
      }

      //每种关系一个阈值
      val validRelations: Set[Int] = validPosValues.map(_._1).toSet
      val relThresholds = Map[Int, Double]()
      val relAccuValid = Map[Int, Int]()
      for (rel <- validRelations) {
        val validPosValuesRel = validPosValues.filter(_._1 == rel).map(_._2).toList
        val validNegValuesRel = validNegValues.filter(_._1 == rel).map(_._2).toList
        val min = (List(validPosValuesRel.min, validNegValuesRel.min).min * 100).toInt
        val max = (List(validPosValuesRel.max, validNegValuesRel.max).max * 100).toInt
        var maxThreshold: Double = 0
        var maxScore = 0
        for (lambda <- min.to(max); threshold = lambda * 0.01) {
          val score = validPosValuesRel.count(_ < threshold) + validNegValuesRel.count(_ >= threshold)
          if (score > maxScore) {
            maxScore = score
            maxThreshold = threshold
          }
        }
        relAccuValid(rel) = maxScore
        relThresholds(rel) = maxThreshold
      }
      val validScore = relAccuValid.values.sum * 1.0 / (validPosValues.size + validNegValues.size)
      println("[验证集]正确率为: " + validScore)

      val testRelations: Set[Int] = testPosValues.map(_._1).toSet
      var testScore = 0
      for (rel <- validRelations) {
        val maxThreshold = relThresholds(rel)
        val testPosValuesRel = testPosValues.filter(_._1 == rel).map(_._2).toList
        val testNegValuesRel = testNegValues.filter(_._1 == rel).map(_._2).toList
        val truePos = testPosValuesRel.count(_ < maxThreshold)
        val trueNeg = testNegValuesRel.count(_ >= maxThreshold)
        testScore += (truePos + trueNeg)
      }

      if ((testRelations -- validRelations).size > 0) {
        val testPosValuesOther = testPosValues.filter(x => !validRelations.contains(x._1)).map(_._2).toList
        val testNegValuesOther = testNegValues.filter(x => !validRelations.contains(x._1)).map(_._2).toList
        val truePos = testPosValuesOther.count(_ < thresholdAll)
        val trueNeg = testNegValuesOther.count(_ >= thresholdAll)
        testScore += (truePos + trueNeg)
      }

      val testFinal = testScore * 1.0 / (testPosValues.size + testNegValues.size)
      println("[测试集]正确率为: " + testFinal)
      testFinal
    }
  }


  def prepare() {
//    println("load entity with id")
    for (line <- Source.fromFile(basePath + corpus + "/data/entity2id.txt", encode).getLines) {
      val terms = line.split("\t")
      entity2id(terms(0)) = terms(1).toInt
      id2entity(terms(1).toInt) = terms(0)
    }
//    println("load relation with id")
    for (line <- Source.fromFile(basePath + corpus + "/data/relation2id.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2id(terms(0)) = terms(1).toInt
      id2relation(terms(1).toInt) = terms(0)
    }
//    println("load relation with relation type")
    for (line <- Source.fromFile(basePath + corpus + "/data/relation2reltype.txt", encode).getLines) {
      val terms = line.split("\t")
      relation2type(terms(0).toInt) = terms(1)
    }
//    println("load high frequent relation")
    val tempRelations = collection.mutable.Set[Int]()
    for ((line, index) <- Source.fromFile(basePath + corpus + "/data/relationFrequent.txt", encode).getLines.zipWithIndex if index < 200) {
      val terms = line.split("\t")
      tempRelations += terms(0).toInt
    }
    highFrqRelations = tempRelations.toSet
  }
}

