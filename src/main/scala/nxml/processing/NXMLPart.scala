package com.wisecube
package nxml.processing

sealed trait NXMLPart extends Product {
  def prettyPrint(maxWidth: Int = 0): String
}

// HasX traits

trait HasText {
  def text: String
  def isEmpty: Boolean = text.isEmpty
}

trait HasNames {
  def names: Seq[String]
}

// Printer traits

trait TextPrinter extends NXMLPart with HasText {
  override def prettyPrint(maxWidth: Int = 0): String = prettyPrintString(text, maxWidth)
}

trait ValuePrinter[T] extends NXMLPart {
  def value: T
  override def prettyPrint(maxWidth: Int = 0): String = prettyPrintString(value.toString, maxWidth)
}

trait NamesPrinter extends NXMLPart with HasNames {
  def sep: String = " "
  override def prettyPrint(maxWidth: Int = 0): String = {
    prettyPrintString(names.mkString(sep), maxWidth)
  }
}

case class ContributorGroup(contributors: Map[Contributor, String] = Map.empty) extends NamesPrinter {
  override val sep: String = " and "
  override val names: Seq[String] = contributors.keys.map(_.prettyPrint()).toSeq
}

case class Contributor(
  names: Seq[String] = Seq.empty,
  affiliation: Seq[String] = Seq.empty
) extends NXMLPart with NamesPrinter

case class Entry(
  contributors: ContributorGroup = ContributorGroup(),
  name: String = "",
  month: Int = 0,
  year: Int = 0,
  attributes: Map[String, String] = Map.empty,
  entryType: String = "Article"
) extends NXMLPart {
  override def prettyPrint(maxWidth: Int): String = {
    val byString = s"contributors = ${contributors.prettyPrint(maxWidth)}"
    val nameString = s"name = ${prettyPrintString(name, maxWidth)}"
    val dateString = "date = " + ((year, month) match {
      case (0, _) => ""
      case (_, 0) => year.toString
      case _ => s"$month-$year"
    })
    val attrStrings = attributes.map { case (k, v) => prettyPrintString(s"$k = $v", maxWidth) }.toList
    (byString :: nameString :: dateString :: attrStrings).mkString("\n")
  }
}

case class Journal(
  names: Seq[String] = Seq.empty,
  journalIds: Map[String, String] = Map.empty
) extends NamesPrinter

case class Reference(
  pubType: String = "",
  entry: Entry = Entry()
) extends NXMLPart {
  override def prettyPrint(maxWidth: Int = 0): String =
    s"""
       |@$pubType2bibtexType{
       |${entry.prettyPrint(maxWidth)}
       |}
       |""".stripMargin

  val pubType2bibtexType: String = entry.getClass.toString.toLowerCase()
}

case class Content(
  title: String = "",
  content: Seq[String]
) extends NXMLPart {
  def addContent(text: String): Content = copy(content = content :+ text)
  def concat(other: Content): Content = copy(content = content ++ other.content)

  override def prettyPrint(maxWidth: Int = 0): String = {
    val sb = StringBuilder.newBuilder
    sb.append(prettyPrintString(title, maxWidth))
    sb.append("\n\n")
    sb.append(content.map(prettyPrintString(_, maxWidth)).mkString("\n\n"))
    sb.toString()
  }
}

case class Document(
  article: Entry = Entry(),
  content: Seq[Content] = Seq.empty,
  references: Seq[Reference] = Seq.empty,
  attributes: Map[String, String] = Map.empty,
  toc: Map[String, Int] = Map.empty
) extends NXMLPart with HasText {

  def addAttribute(pairs: (String, String)*): Document = copy(attributes = attributes ++ pairs)

  def addSection(section: Content): Document = {
    val withTitle: Content = if (section.title.isEmpty) {
      section.copy(title = s"Section ${content.size}")
    } else { section }
    copy(
      content = content :+ withTitle,
      toc = toc + (withTitle.title -> content.size)
    )
  }

  def addContent(part: Content): Document = copy(content = content :+ part)

  def addReferences(newReferences: Reference*): Document = copy(references = references ++ newReferences)

  def prettyPrint(maxWidth: Int = 0): String = {
    val sb = StringBuilder.newBuilder

    sb.append(article.prettyPrint(maxWidth))

    if (getToC.nonEmpty) {
      sb.append("\n\n")
      sb.append(getToC.map { case (sec, i) => s"$i - ${sec}" }.mkString("\n"))
    }
    sb.append("\n\n")
    sb.append(content.map(_.prettyPrint(maxWidth)).mkString("\n\n"))
    if (references.nonEmpty) {
      sb.append("\n\n")
      sb.append("References")
      sb.append("\n\n")
      sb.append(references.map(_.prettyPrint(maxWidth)).mkString("\n"))
    }
    if (attributes.nonEmpty) {
      sb.append("\n\n")
      sb.append(attributes.map { case (k, v) => s"[$k] = [$v]" }.mkString("\n"))
    }
    sb.toString()
  }

  def getToC: Seq[(String, Int)] = toc.toSeq.sortBy(_._2)

  override def text: String = content.map(_.prettyPrint()).mkString("\n\n")
}

object Document {
  val empty: Document = Document(Entry(), Seq.empty, Seq.empty, Map.empty, Map.empty)
}