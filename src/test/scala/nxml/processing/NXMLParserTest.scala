package com.wisecube
package nxml.processing
import java.io.File

object NXMLParserTest {
  def main(args: Array[String]): Unit = {
    val samples = new File("./src/test/resources/pubmed-sample/").listFiles().headOption
    samples.foreach {
      file =>
        println(file)
        val article = loadNXML(file)
        val doc = NXMLParser.parseNXML(article)
        println(doc.article)
        println(doc.toc)
        println(doc.content)
        println(doc.references)
        println(doc.attributes)
    }
  }
}
