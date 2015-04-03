package org.nlpr.cip.kb.eval

import org.apache.log4j.Logger

import scala.collection.mutable.{ArrayBuffer, Map}
import scala.io.Source
;

/**
 * User: hesz
 * Date: 2015-1-8
 * Time: 16:57
 */
object TransModel {
  var normalize_flag = false

  var basePath: String = ""
  var corpus: String = "fb15k-exp"
  var model: String = "TransJR_L1"
  var negType: String = "bern"
  var size: Int = 50
  var l1: Boolean = true


  def main(args: Array[String]) {
    val model = TransModel("fb15k-exp", "TransJR_L1")

  }

  def apply(corpus:String, model:String): TransModel = {
    apply(corpus, model, "bern")
  }

  def apply(corpus:String, model:String, size: Int): TransModel = {
    apply(corpus, model, "bern", size)
  }

  def apply(corpus:String, model:String, negType:String): TransModel = {
    apply(corpus, model, negType, 50)
  }

  def apply(corpus:String, model:String, negType:String, size:Int): TransModel = {
    apply(basePath, corpus, model, negType, size, if(model.endsWith("L1")) true else false)
  }

  def apply(basePath:String, corpus:String, model:String, negType:String, size:Int): TransModel = {
    apply(basePath, corpus, model, negType, size,if(model.endsWith("L1")) true else false)
  }

  def apply(basePath:String, corpus:String, model:String, negType:String, size:Int, l1: Boolean): TransModel = {
    TransModel.basePath = basePath
    TransModel.corpus = corpus
    TransModel.model = model
    TransModel.negType = negType
    TransModel.size = size
    TransModel.l1 = l1
    if(model.startsWith("GEEL"))
      return getGEEL()
    if(model.startsWith("GEKL"))
      return getGEKL()
    if(model.startsWith("BidirH"))
      return getBidirH()
    if(model.startsWith("JointH"))
      return getJointH()
    if(model.startsWith("TransHBi"))
      return getJointH()
    if(model.startsWith("JointP2"))
      return getTransHBi()
    if (model.startsWith("TransE"))
      return getTransE()
    if (model.startsWith("TransH"))
      return getTransH()
    else if (model.startsWith("TransR"))
      return getTransR()
    else if (model.startsWith("TransJ"))
      return getTransJ()
    else if (model.startsWith("JH"))
      return getTransJH()
    else if (model.startsWith("J"))
      return getTransJ()
    else new EmptyTransModel()
  }

  def getTransE(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    if(l1) new TransEL1(entity2vec, relation2vec)
    else new TransEL2(entity2vec, relation2vec)
  }

  def getTransH(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val relation2vecHyper = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath2 = "%s%s/%s/A.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath2).getLines.zipWithIndex if line.trim.length > 0)
      relation2vecHyper(id) = line.split("\t").map(_.toDouble)
    if(l1) new TransHL1(entity2vec, relation2vec, relation2vecHyper)
    else new TransHL2(entity2vec, relation2vec, relation2vecHyper)
  }

  def getTransR(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    //读取关系矩阵，共有每读n行为一个关系
    val matrixPath = "%s%s/%s/A.%d.%s".format(basePath, corpus, model, size, negType)
    val relation2mat = Map[Int, Array[Array[Double]]]()
    var tempMatrix = ArrayBuffer[Array[Double]]()
    var rel_id = 0
    for((line, index) <- Source.fromFile(matrixPath).getLines.zipWithIndex) {
      if((index != 0) && (index % size == 0)){
        relation2mat(rel_id) = tempMatrix.toArray
        tempMatrix = ArrayBuffer[Array[Double]]()
        rel_id += 1
      }
      tempMatrix += line.split("\t").map(_.toDouble)
    }
    relation2mat(rel_id) = tempMatrix.toArray
    if(l1) new TransRL1(entity2vec, relation2vec, relation2mat)
    else new TransRL2(entity2vec, relation2vec, relation2mat)
  }

  def getTransJ(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    //读取关系矩阵，共有每读n行为一个关系
    val matrixPath = "%s%s/%s/relation2mat.%d.%s".format(basePath, corpus, model, size, negType)
    val relation2mat = Map[Int, Array[Array[Double]]]()
    var tempMatrix = ArrayBuffer[Array[Double]]()
    var rel_id = 0
    for((line, index) <- Source.fromFile(matrixPath).getLines.zipWithIndex) {
      if((index != 0) && (index % size == 0)){
        relation2mat(rel_id) = tempMatrix.toArray
        tempMatrix = ArrayBuffer[Array[Double]]()
        rel_id += 1
      }
      tempMatrix += line.split("\t").map(_.toDouble)
    }
    relation2mat(rel_id) = tempMatrix.toArray
    if(l1) new TransJL1(entity2vec, relation2mat)
    else new TransJL2(entity2vec, relation2mat)
  }

  def getTransJH(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    if(l1) new TransJHL1(entity2vec, relation2vec)
    else new TransJHL2(entity2vec, relation2vec)
  }

  def getTransHBi(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    if(l1) new TransHBiL1(entity2vec, relation2vec)
    else new TransHBiL2(entity2vec, relation2vec)
  }

  def getJointH(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    if(l1) new JointHL1(entity2vec, relation2vec)
    else new JointHL2(entity2vec, relation2vec)
  }

  def getBidirH(): TransModel = {
    val entity2vec = Map[Int, Array[Double]]()
    val relation2vec = Map[Int, Array[Double]]()
    val relation2hyper = Map[Int, Array[Double]]()
    val entVecPath = "%s%s/%s/entity2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(entVecPath).getLines.zipWithIndex if line.trim.length > 0)
      entity2vec(id) = line.split("\t").map(_.toDouble)
    val relVecPath = "%s%s/%s/relation2vec.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relVecPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2vec(id) = line.split("\t").map(_.toDouble)
    val relHyperPath = "%s%s/%s/relation2hpyer.%d.%s".format(basePath, corpus, model, size, negType)
    for ((line, id) <- Source.fromFile(relHyperPath).getLines.zipWithIndex if line.trim.length > 0)
      relation2hyper(id) = line.split("\t").map(_.toDouble)
    if(l1) new BidirHL1(entity2vec, relation2vec, relation2hyper)
    else new BidirHL2(entity2vec, relation2vec, relation2hyper)
  }

  def getGEKL(): TransModel = {
    val ent2mean = Map[Int, Array[Double]]()
    val ent2vari = Map[Int, Array[Double]]()
    val rel2mean = Map[Int, Array[Double]]()
    val rel2vari = Map[Int, Array[Double]]()
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
    new GEKL(ent2mean, ent2vari, rel2mean, rel2vari)
  }

  def getGEEL(): TransModel = {
    val ent2mean = Map[Int, Array[Double]]()
    val ent2vari = Map[Int, Array[Double]]()
    val rel2mean = Map[Int, Array[Double]]()
    val rel2vari = Map[Int, Array[Double]]()
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
    new GEEL(ent2mean, ent2vari, rel2mean, rel2vari)
  }
}

abstract class TransModel {

  def vecDot(a: Array[Double], b: Array[Double]): Double = (a zip b).map(x => x._1 * x._2).sum

  def vecAdd(a: Array[Double], b: Array[Double]): Array[Double] = (a zip b).map(x => x._1 + x._2)

  def vecSub(a: Array[Double], b: Array[Double]): Array[Double] = (a zip b).map(x => x._1 - x._2)

  def multiply(m: Array[Array[Double]], v: Array[Double]): Array[Double] = m.map(vecDot(_, v))


  def mapHyper(v: Array[Double], h: Array[Double]): Array[Double] = {
    val inner = vecDot(v, h)
    (v zip h).map(x => x._1 - inner * x._2)
  }

  def vecNorm_1(a: Array[Double]): Double = a.map(math.abs(_)).sum

  def vecNorm_2(a: Array[Double]): Double = math.sqrt(a.map(x => x * x).sum)

  def l1_distance(a: Array[Double], b: Array[Double]): Double =
    if(TransModel.normalize_flag) {
      vecNorm_1(vecSub(normalize(a), normalize(b)))
    } else {
      vecNorm_1(vecSub(a, b))
    }

  def l2_distance(a: Array[Double], b: Array[Double]): Double =
    if(TransModel.normalize_flag) vecNorm_2(vecSub(normalize(a), normalize(b))) else vecNorm_2(vecSub(a, b))

  def normalize(a: Array[Double]): Array[Double] = {
    val norm_2 = math.sqrt(a.map(x => x * x).sum)
    if (norm_2 > 0) a.map(x => x / norm_2) else a
  }

  def distance(h: Int, r: Int, t: Int): Double //L1/L2
}

class EmptyTransModel extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = 0.5f
}

//TransE模型
class TransEL1(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    l1_distance(vecAdd(h_vec, r_vec), t_vec)
  }
}

class TransEL2(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    l2_distance(vecAdd(h_vec, r_vec), t_vec)
  }
}

//TransH模型
class TransHL1(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]],
               val relation2vecHyper: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec_hyper = relation2vecHyper(r)
    val r_h_vec = mapHyper(h_vec, r_vec_hyper)
    val r_t_vec = mapHyper(t_vec, r_vec_hyper)
    val r_vec = relation2vec(r)
    l1_distance(vecAdd(r_h_vec, r_vec), r_t_vec)
  }
}

class TransHL2(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]],
               val relation2vecHyper: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec_hyper = relation2vecHyper(r)
    val r_h_vec = mapHyper(h_vec, r_vec_hyper)
    val r_t_vec = mapHyper(t_vec, r_vec_hyper)
    val r_vec = relation2vec(r)
    l2_distance(vecAdd(r_h_vec, r_vec), r_t_vec)
  }
}

//TransR模型
class TransRL1(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]],
               val relation2matrix: Map[Int, Array[Array[Double]]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    val r_mat = relation2matrix(r)
    val h_r_vec = multiply(r_mat, h_vec)
    val t_r_vec = multiply(r_mat, t_vec)
    l1_distance(vecAdd(h_r_vec, r_vec), t_r_vec)
  }
}

class TransRL2(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]],
               val relation2matrix: Map[Int, Array[Array[Double]]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    val r_mat = relation2matrix(r)
    val h_r_vec = multiply(r_mat, h_vec)
    val t_r_vec = multiply(r_mat, t_vec)
    l2_distance(vecAdd(h_r_vec, r_vec), t_r_vec)
  }
}

//TransG模型
class TransJL1(val entity2vec: Map[Int, Array[Double]],
               val relation2matrix: Map[Int, Array[Array[Double]]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_mat = relation2matrix(r)
    val t_r_vec = multiply(r_mat, t_vec)
    l1_distance(normalize(h_vec), normalize(t_r_vec))
  }
}

class TransJL2(val entity2vec: Map[Int, Array[Double]],
               val relation2matrix: Map[Int, Array[Array[Double]]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_mat = relation2matrix(r)
    val t_r_vec = multiply(r_mat, t_vec)
    l2_distance(h_vec, t_r_vec)
  }
}

//TransJH模型
class TransJHL1(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    val r_t_vec = mapHyper(t_vec, r_vec)
    l1_distance(h_vec, r_t_vec)
  }
}

class TransJHL2(val entity2vec: Map[Int, Array[Double]],
               val relation2vec: Map[Int, Array[Double]]
                ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val h_vec = entity2vec(h)
    val t_vec = entity2vec(t)
    val r_vec = relation2vec(r)
    val r_t_vec = mapHyper(t_vec, r_vec)
    l2_distance(h_vec, r_t_vec)
  }
}

//TransHBi
class TransHBiL1(val entity2vec: Map[Int, Array[Double]],
                val relation2vec: Map[Int, Array[Double]]
                 ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size / 2
    val h_vec = mapHyper(entity2vec(h), relation2vec(r))
    val t_vec = mapHyper(entity2vec(t), relation2vec(r + rel_num))
    l1_distance(h_vec, t_vec)
  }
}

class TransHBiL2(val entity2vec: Map[Int, Array[Double]],
                val relation2vec: Map[Int, Array[Double]]
                 ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size / 2
    val h_vec = mapHyper(entity2vec(h), relation2vec(r))
    val t_vec = mapHyper(entity2vec(t), relation2vec(r + rel_num))
    l2_distance(h_vec, t_vec)
  }
}

//JointH
class JointHL1(val entity2vec: Map[Int, Array[Double]],
                 val relation2vec: Map[Int, Array[Double]]
                  ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size / 2

    val h_vec = mapHyper(entity2vec(h), relation2vec(r))
    val loss1 = l1_distance(h_vec, entity2vec(t))

    val t_vec = mapHyper(entity2vec(t), relation2vec(r + rel_num))
    val loss2 = l1_distance(t_vec, entity2vec(h))

    (loss1 + loss2) / 2
  }
}

class JointHL2(val entity2vec: Map[Int, Array[Double]],
                 val relation2vec: Map[Int, Array[Double]]
                  ) extends TransModel {
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size / 2

    val h_vec = mapHyper(entity2vec(h), relation2vec(r))
    val loss1 = l2_distance(h_vec, entity2vec(t))

    val t_vec = mapHyper(entity2vec(t), relation2vec(r + rel_num))
    val loss2 = l2_distance(t_vec, entity2vec(h))

    (loss1 + loss2) / 2
  }
}

//BidirH
class BidirHL1(val entity2vec:Map[Int, Array[Double]],
                val relation2vec:Map[Int, Array[Double]],
                val relation2hyper:Map[Int, Array[Double]]) extends TransModel{
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size
    val h_vec = mapHyper(entity2vec(h), relation2hyper(r))
    val t_vec = mapHyper(entity2vec(t), relation2hyper(r + rel_num))
    l1_distance(vecAdd(h_vec, relation2vec(r)), t_vec)
  }
}

class BidirHL2(val entity2vec:Map[Int, Array[Double]],
               val relation2vec:Map[Int, Array[Double]],
               val relation2hyper:Map[Int, Array[Double]]) extends TransModel{
  def distance(h: Int, r: Int, t: Int): Double = {
    val rel_num = relation2vec.size
    val h_vec = mapHyper(entity2vec(h), relation2hyper(r))
    val t_vec = mapHyper(entity2vec(t), relation2hyper(r + rel_num))
    l2_distance(vecAdd(h_vec, relation2vec(r)), t_vec)
  }
}

//GEKL
class GEKL(val ent2mean:Map[Int, Array[Double]],
           val ent2vari:Map[Int, Array[Double]],
           val rel2mean:Map[Int, Array[Double]],
           val rel2vari:Map[Int, Array[Double]])extends TransModel{
  def distance(h: Int, r: Int, t: Int): Double = {
    var score = 0.0
    val n = ent2mean(h).length
    for(i <- 0 until n ){
     score += (ent2vari(h)(i) + ent2vari(t)(i)) / rel2vari(r)(i)
     val tt = (ent2mean(h)(i) - ent2mean(t)(i) - rel2mean(r)(i))
     score += tt * tt / rel2vari(r)(i)
     score += math.log(rel2vari(r)(i))
     score -= math.log(ent2vari(h)(i) + ent2vari(t)(i))
    }
    score / 2
  }
}

//GEEL
class GEEL(val ent2mean:Map[Int, Array[Double]],
           val ent2vari:Map[Int, Array[Double]],
           val rel2mean:Map[Int, Array[Double]],
           val rel2vari:Map[Int, Array[Double]])extends TransModel{
  def distance(h: Int, r: Int, t: Int): Double = {
    var score = 0.0
    val n = ent2mean(h).length
    for(i <- 0 until n ){
      val mean = ent2mean(h)(i) - rel2mean(r)(i) - ent2mean(t)(i)
      val vari = ent2vari(h)(i) + rel2vari(r)(i) + ent2vari(t)(i)
      score += math.log(vari)
      score += mean * mean / vari
    }
    -0.5 *score
  }
}

