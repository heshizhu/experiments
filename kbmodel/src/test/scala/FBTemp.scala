import java.io.PrintWriter

import scala.io.Source
import scala.collection.mutable.{Map}


object FBTemp {

  def main(args: Array[String]) {

    val path_one = "G:\\temp\\TransX\\fb15k\\data/entity2id.txt"
    val entities = Source.fromFile(path_one, "utf-8").getLines.map(_.split("\t")(0)).toSet
    println("len: " + entities.size)

    val ent_ment = Map[String, String]()

    val path_two = "G:\\temp\\freebase\\entity_name.txt"
    for(line <- Source.fromFile(path_two, "utf-8").getLines){
      val terms = line.split("\t")
      val (ent, ment) = (terms(0), terms(1))
      val entnew = "/" + ent.split("\\.").mkString("/")
      if(entities.contains(entnew)
        && ment.startsWith("\"") && ment.endsWith("\"@en")){
        val mentnew = ment.substring(1, ment.length - 4)
        ent_ment(entnew) = mentnew
      }
    }

    println("valid len: " + ent_ment.size)
    val writer = new PrintWriter("G:\\temp\\freebase\\entity_name_mini.txt", "utf-8")
    for((sub, obj) <- ent_ment.init)
      writer.write("%s\t%s\n".format(sub, obj))
    writer.close()
  }
}
