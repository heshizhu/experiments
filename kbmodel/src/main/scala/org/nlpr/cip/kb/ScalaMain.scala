package org.nlpr.cip.kb


import java.io.PrintWriter

import scala.io.Source

/**
 * User: hesz
 * Date: 2015-3-6
 * Time: 10:54
 */
class ScalaMain {

}

object ScalaMain{
  def main(args: Array[String]) {
    val base_path = "G:\\temp\\TransX\\fb13\\"
    val form_file = base_path + "data/rules.txt"

    val rel_set = scala.collection.mutable.Set[String]()
    for(line <- Source.fromFile(form_file).getLines if (line.trim.length > 0 && !line.startsWith("#"))){
      for((term, id) <- line.split("[\t| ]").zipWithIndex if (id % 3 == 1))
        rel_set += term
    }

    for(name <- List("test_neg_bern","test_neg_unif", "test_pos_bern", "test_pos_unif",
                    "valid_neg_bern", "valid_neg_unif", "valid_pos_bern", "valid_pos_unif")){
      val input = base_path + "data/" + name + ".txt"
      val out = new PrintWriter(base_path + "data/" + name + ".new.txt")
      for(line <- Source.fromFile(input).getLines){
        val rel = line.split("[\t| ]")(1)
        if(rel_set.contains(rel))
          out.println(line)
      }

      out.close()
    }
  }
}
