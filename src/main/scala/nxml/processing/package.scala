package com.wisecube
package nxml

import java.io.File
import java.nio.file.Path
import scala.xml.Node
import scala.annotation.tailrec

package object processing {
  def loadNXML(path: String): Node = xml.XML.load(path)
  def loadNXML(path: Path): Node = xml.XML.loadFile(path.toFile)
  def loadNXML(file: File): Node = xml.XML.loadFile(file)
  def loadNXMLString(nxml: String): Node = xml.XML.loadString(nxml)

  def prettyPrintString(text: String, maxWidth: Int): String = if (maxWidth > 0) {
    var sb = StringBuilder.newBuilder
    val (tail, _) = text.foldLeft(("", 0)) {
      case ((acc: String, lastWSIx: Int), c: Char) =>
        if (acc.length < maxWidth && c.isWhitespace) {
          (acc + c, acc.length)
        } else if (acc.length < maxWidth && !c.isWhitespace) {
          (acc + c, lastWSIx)
        } else if (acc.length == maxWidth && c.isWhitespace) {
          sb.append(acc + "\n")
          ("", 0)
        } else if (acc.length == maxWidth && !c.isWhitespace && lastWSIx < maxWidth * 0.75) {
          sb.append(acc + "-\n")
          (c.toString, 0)
        } else if (c.isWhitespace) {
          sb.append(acc.take(lastWSIx+1) + "\n")
          (acc.drop(lastWSIx) + c, acc.length - lastWSIx + 1)
        } else {
          sb.append(acc.take(lastWSIx+1) + "\n")
          (acc.drop(lastWSIx + 1) + c, 0)
        }
    }
    sb.append(tail)
    sb.mkString
  } else { text }

  def getData(elem: Node): Iterable[String] = {
    @tailrec
    def inner(acc: Seq[String], passing: Seq[Node]): Seq[String] = {
      if (passing.isEmpty) {
        acc
      } else {
        val head  +: tail = passing
        if (elem.label == "#PCDATA") {
          inner(acc :+ head.text, tail)
        } else {
          inner(acc, elem.child.toList ++ passing)
        }
      }
    }
    inner(Seq.empty, Seq(elem))
  }

  def getEntry(pubType: String): String =
    pubType match {
      case "article" | "ejournal" | "journal" | "newspaper" | "online-journal" | "joural" | "periodical"  => "Article"
      case "book" | "institutional book" => "Book"
      case "incollection" | "chapter" | "inbook" => "InCollection"
      case "inproceedings" | "conference-paper" => "InProceedings"
      case "manual" => "Manual"
      case "masterthesis" => "MasterThesis"
      case _ => "Misc"
      case "phdthesis" | "thesis" => "PhdThesis"
      case "proceedings" | "conf-proc" | "confrpoc" | "conference" | "confproc" | "conf" => "Proceedings"
      case "techreport" | "report" => "TechReport"
      case "unpublished" | "preprint" | "working-paper" => "Unpublished"
    }
}
