package org.nlpr.cip.kb.corpus

import java.io.{File, PrintWriter}
import java.nio.charset.Charset

import com.google.common.io.Files
/**
 * User: hesz
 * Date: 2015-3-23
 * Time: 12:30
 */
object freebase {

  def main(args: Array[String]) {
    val fb_dump_path = "F:\\temp\\freebase\\freebase-rdf-2014-06-29-00-00"
    val writer = new PrintWriter("G:\\temp\\freebase\\embedding\\type_instance.txt", "utf-8")
    val reader = Files.newReader(new File(fb_dump_path), Charset.forName("utf-8"))
    var line = ""
    while((line = reader.readLine()) != null){
      val terms = line.split("\t")
      if(terms.length > 3 &&
        terms(1).equals("<http://rdf.freebase.com/ns/type.object.type>") &&
        terms(0).startsWith("<http://rdf.freebase.com/ns/m.") &&
        !terms(2).equals("<http://rdf.freebase.com/ns/common.topic>")){
        writer.write(line + "\n")
      }
    }
    writer.close()
    reader.close()
  }
}
