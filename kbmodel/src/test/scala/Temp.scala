
import scala.collection.immutable.ListMap
import scala.collection.mutable.{Set, Map}
import scala.io.Source
import scala.util.Random

/**
 * User: hesz
 * Date: 2015-3-6
 * Time: 12:21
 */
object Temp {
  def main(args: Array[String]) {
    //统计freebase中人物领域的实体
    //<http://rdf.freebase.com/ns/people.person.*>
    val entites = Set[String]()
    entites.add("0d06m5")
    entites.add("032dj0")



    val rel_heads = Map[String, Set[String]]()
    val rel_tails = Map[String, Set[String]]()

    val ent_frq = Map[String, Int]()
    val rel_frq = Map[String, Int]() //前面是关系，后面是关系出现的三元组个数


    var count = 0

    val entpatt = "<http://rdf.freebase.com/ns/m.(.*?)>".r
    val relpatt = "<http://rdf.freebase.com/ns/people.person.(.*?)>".r

    for(line <- Source.fromFile("G:\\temp\\freebase\\freebase-retain", "utf-8").getLines) {
      val terms = line.split("\t")
      if(terms.size >= 3 &&
        terms(0).startsWith("<http://rdf.freebase.com/ns/m.")
          &&terms(1).startsWith("<http://rdf.freebase.com/ns/people.person.")){
        val entpatt(sub) = terms(0)
        val relpatt(rel) = terms(1)
        val obj = if(terms(2).startsWith("<http://rdf.freebase.com/ns/m.")) {
          val entpatt(xxx) = terms(2)
          xxx
        } else terms(2)

//        if(entites.contains(sub))
//          ent_frq(sub) = ent_frq.getOrElse(sub, 0) + 1
//        else if(entites.size < 100 && Random.nextInt(100) == 1){
//          entites.add(sub)
//          ent_frq(sub) = ent_frq.getOrElse(sub, 0) + 1
//        }
        ent_frq(sub) = ent_frq.getOrElse(sub, 0) + 1

        rel_frq(rel) = rel_frq.getOrElse(rel, 0) + 1

        val heads = rel_heads.getOrElse(rel, Set[String]())
        heads.add(sub)
        rel_heads(rel) = heads
        val tails = rel_tails.getOrElse(rel, Set[String]())
        tails.add(obj)
        rel_tails(rel) = tails
      }
      count += 1
      if((count % 10000000) == 0) println(count)
    }


    val rel_head_frq = Map[String, Int]()
    val rel_tail_frq = Map[String, Int]()
    for(rel <- rel_heads.keys) {
      rel_head_frq(rel) = rel_heads(rel).size
      rel_tail_frq(rel) = rel_tails(rel).size
    }

    val ent_frq_sort = ListMap(ent_frq.toSeq.sortWith(_._2 > _._2):_*)
    println("实体的三元组分布(从大到小排列)")
    for((ent, frq) <- ent_frq_sort.iterator)
      println(ent + "\t" + frq)
    println("\n\n\n")

    val rel_frq_sort = ListMap(rel_frq.toSeq.sortWith(_._2 > _._2):_*)
    println("关系包含的三元组分布(从大到小排列)")
    for((rel, frq) <- rel_frq_sort.iterator)
      println(rel + "\t" + frq)
    println("\n\n\n")


    val rel_head_tail_ratio = Map[String, Double]()
    for(rel <- rel_head_frq.keys)
      rel_head_tail_ratio(rel) = rel_head_frq(rel) * 1.0 / (rel_head_frq(rel) + rel_tail_frq(rel))
    val rel_head_tail_ratio_sort = ListMap(rel_head_tail_ratio.toSeq.sortWith(_._2 > _._2):_*)
    println("关系前后比例")
    for((rel, ratio) <- rel_head_tail_ratio_sort)
      println("%s\t%f\t%f\t%d\t%d".format(rel, ratio, 1 - ratio, rel_head_frq(rel), rel_tail_frq(rel)))
    println("\n\n\n")
  }
}
