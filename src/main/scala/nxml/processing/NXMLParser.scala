package com.wisecube
package nxml.processing

import scala.xml.Node

import scala.util.Try

object NXMLParser {

  def parseNXML(article: Node): Document = {
    val front: Node = (article \ "front").head
    val body: Option[Node] = (article \ "body").headOption
    val back: Option[Node] = (article \ "back").headOption
    val frontDoc = parseFront(Document.empty, front)
    val bodyDoc = body.map(parseBody(frontDoc, _)).getOrElse(frontDoc)
    bodyDoc.addReferences(back.map(parseBack(Seq.empty, _)).getOrElse(Seq.empty): _*)
  }

  def parseFront(doc: Document, elem: Node): Document =
    elem.label match {
      case "front" => elem.child.foldLeft(Document.empty)((d, e) => parseFront(d, e))
      case "journal-meta" => parseJournalMeta(doc, elem)
      case "article-meta" => parseArticleMeta(doc, elem)
      case _ => Document.empty
    }

  def parseBody(doc: Document, elem: Node): Document =
    elem.label match {
      case "body" => elem.child.foldLeft(doc)((d, e) => parseBody(d, e))
      case "p" => doc.addContent(Content(content = Seq(elem.text)))
      case "sec" =>
        val title = (elem \ "title").headOption.map(_.text).getOrElse("")
        val section = Content(title, Seq.empty)
        doc.addSection(parseSection(section, elem))
      case _ => doc
    }

  def parseSection(section: Content, elem: Node): Content =
    elem.label match {
      case "p" => section.addContent(elem.text)
      case "sec" =>
        val title = (elem \ "title").headOption.map(_.text).getOrElse("")
        val subsection = Content(title, Seq.empty)
        elem.child.foldLeft(subsection)((ss, e) => parseSection(ss, e))
      case _ => section
    }

  def parseBack(refs: Seq[Reference] = Seq.empty, elem: Node): Seq[Reference] =
    elem.label match {
      case "back" => elem.child.foldLeft(refs)((rs, e) => parseBack(rs, e))
      case "ref-list" => (elem \ "ref" ).map(_.child).flatten.foldLeft(refs)((rs, e) => parseBack(rs, e))
      case "nlm-citation" | "element-citation" | "mixed-citation" =>
        val pubType = elem \@ "publication-type"
        val entry: Entry = parseEntry(pubType, elem)
        refs :+ Reference(pubType, entry)
      case _ => refs
    }

  def parseEntry(pubType: String, elem: Node): Entry = {
    val contributors: ContributorGroup = ContributorGroup()
    val title: String = (elem \ "article-title").headOption.map(_.text).getOrElse("")
    val year: Int = (elem \ "year").headOption.flatMap(y => Try[Int](y.text.toInt).toOption)
      .getOrElse(0)
    val month: Int = (elem \ "month").headOption.flatMap(m => Try[Int](m.text.toInt).toOption)
      .getOrElse(0)
    getEntry(pubType) match {
      case et @ "Article" =>
        val journal = ((elem \ "source") ++ (elem \ "series")).headOption
          .map(s => Journal(Seq(s.text))).getOrElse(Journal()).prettyPrint()
        val volume = (elem \ "volume").headOption.map(_.text).getOrElse("")
        Entry(contributors, title, month, year, Map("journal" -> journal, "volume" -> volume), et)
      case et @ "Book" =>
        val publisher = Contributor((elem \ "publisher-name").map(_.text)).prettyPrint()
        Entry(contributors, title, month, year, Map("publisher" -> publisher), et)
      case et @ "InCollection" =>
        val bookTitle = (elem \ "source").headOption.map(_.text).getOrElse("")
        val publisher = Contributor((elem \ "publisher-name").map(_.text)).prettyPrint()
        Entry(contributors, title, month, year, Map("publisher" -> publisher, "bookTitle" -> bookTitle), et)
      case et @ "InProceedings" =>
        val bookTitle = (elem \ "source").headOption.map(_.text).getOrElse("")
        Entry(contributors, title, month, year, Map("bookTitle" -> bookTitle), et)
      case et =>
        Entry(contributors, title, month, year, entryType = et)
    }
  }

  def parseJournalMeta(doc: Document, elem: Node): Document = {
    val journalIds = (elem \ "journal-id").map {
      jid: xml.Node =>
        "journal-id:%s".format(jid \@ "journal-id-type") -> jid.text
    }.toMap[String, String]
    doc.copy(attributes = doc.attributes ++ journalIds)
  }

  def parseArticleMeta(doc: Document, articleMeta: Node): Document = {
    val articleIds = (articleMeta \ "article-id").map {
      aid: xml.Node =>
        "article-id:%s".format(aid \@ "pub-id-type") -> aid.text
    }.toMap[String, String]
    val abstractSection = Content(
      title = (articleMeta \ "abstract" \ "title").headOption.map(_.text).getOrElse("Abstract"),
      content = (articleMeta \ "abstract" \ "p").map(_.text)
    )
    val article: Entry = articleMeta.child.foldLeft(Entry(entryType = "Article")) {
      case (articleEntry: Entry, elem: Node) =>
        elem.label match {
          case "title-group" =>
            val title: String = (elem \ "article-title").text
            articleEntry.copy(name = title)
          case "contrib-group" =>
            articleEntry.copy(contributors = parseContributors(ContributorGroup(), elem))
          case _ => articleEntry
        }
    }
    doc.copy(article = article).addAttribute(articleIds.toSeq: _*).addSection(abstractSection)
  }

  def parseContributors(contributorGroup: ContributorGroup, elem: Node): ContributorGroup = {
    elem.label match {
      case "contrib-group" | "person-group" =>
        elem.child.foldLeft(contributorGroup)((cg, e) => parseContributors(cg, e))
      case "contrib" =>
        contributorGroup.copy(contributors = contributorGroup.contributors + parseContributor(elem))
      case "collab" =>
        contributorGroup.copy(contributors = contributorGroup.contributors + parseOrganization(elem))
      case _ => contributorGroup
    }
  }

  def parseOrganization(elem: Node): (Contributor, String) = {
    val institutions = (elem \ "institution")
    if (institutions.nonEmpty) {
      (Contributor(institutions.map(_.text)) -> "institution")
    } else {
      (Contributor(elem.child.collectFirst { case e if e.label == "#PCData" => e.text}.toSeq) -> "organization")
    }
  }

  def parseContributor(elem: Node): (Contributor, String) = {
    val contribType: String = (elem \ "role").headOption match {
      case Some(role) => role.text
      case None => elem.attribute("contrib-type").map(_.text).getOrElse("contributor")
    }
    val style = elem.attribute("name-style").map(_.text).getOrElse("western")
    val name: Seq[String] = (elem \ "name").flatMap(parseName)
    val (aff: Contributor, _) = (elem \ "aff").headOption.map(parseOrganization).getOrElse((Contributor(), ""))
    (Contributor(name, aff.names) -> contribType)
  }

  def parseName(elem: Node): Seq[String] =
    elem.label match {
      case "name" | "string-name" if elem.child.nonEmpty =>
        val surname = (elem \ "surname").headOption.map(_.text).getOrElse("")
        val givenNames = (elem \ "given-names").headOption.map(_.text).getOrElse("")
        val prefix = (elem \ "prefix").headOption.map(_.text).getOrElse("")
        val suffix = (elem \ "suffix").headOption.map(_.text).getOrElse("")
        val style = elem.attribute("name-style").map(_.text).getOrElse("western")
        if (style == "western") {
          prefix :: surname :: givenNames :: suffix :: Nil
        } else {
          prefix :: givenNames :: surname :: suffix :: Nil
        }
      case "string-name" =>
        elem.child.map(_.text).toSeq
    }
}
