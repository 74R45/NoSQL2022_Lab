package x74r45

import x74r45.model.*

import scala.io.Source
import scala.util.matching.Regex

object Parsing {
  val gp: FormatInfo = FormatInfo("//", Map(
    "id" -> between(".*VERSION     ", "KEYWORDS.*"),
    "isolate" -> between(".*isolate=\"", "\".*"),
    "haplogroup" -> between(".*haplogroup=\"", "\".*"),
    "isolation_source" -> between(".*isolation_source=\"", "\".*"),
    "country" -> between(".*country=\"", "\".*")
  ))
  val fasta: FormatInfo = FormatInfo(">", Map(
    "id" -> between("", " Homo.*"),
    "sequence" -> between(".*mitochondrial", "")
  ))

  def parseData(dbs: Seq[(String, String)]): Seq[MtDNASequence] =
    dbs.flatMap(parseDb)

  def parseDb(dbName: String, fileName: String): Array[MtDNASequence] =
    val dataFasta = loadResource(s"$fileName.fasta")
    val dataGp = loadResource(s"$fileName.gp")
    val sequences = parseFasta(dataFasta)
    parseGp(dataGp, dbName, sequences)

  def parseFasta(s: String): Map[String, String] =
    s.split(fasta.sep)
      .filterNot(_.isBlank)
      .map(seqData =>
        val id = fasta.find("id", seqData)
        val seq_raw = fasta.find("sequence", seqData)
        val seq = if (Main.UnifySequences) processSequence(seq_raw) else seq_raw
        (id, seq)
      ).toMap

  def parseGp(s: String, db: String, sequences: Map[String, String]): Array[MtDNASequence] =
    val parsed = s.split(gp.sep)
      .filterNot(_.isBlank)
      .map(seqData =>
        val attributes = Vector("id", "isolate", "haplogroup", "isolation_source", "country")
          .map(gp.find(_, seqData))
        val regionCode = processRegionCode(attributes(1))
        val country = attributes(4).split(": ")(0)
        val haplogroup = attributes(2).replaceAll("\\s+", " ").split("[()]")
        val region =
          if (db == "Ukraine") attributes(4).split(": ") match
            case Array(_) => ""
            case arr => arr(1).replaceAll("\\s+", " ")
          else attributes(3)
        val sequence = sequences(attributes(0))

        MtDNASequence(
          id=attributes(0),
          isolate=attributes(1),
          carriers=1,
          haplogroup=haplogroup(0).strip(),
          haplogroupComment=haplogroup.applyOrElse(1, _ => ""),
          db=db,
          regionCode=regionCode,
          country=country,
          region=region,
          sequence=sequence
        )
      )
    removeUnnecessarySeq(parsed)

  def groupAttributes(seqData: Seq[MtDNASequence]): List[(String, String, String, Set[String])] =
    List(
      ("db", "DB", "FROM_DB", seqData.map(_.db).toSet),
      ("regionCode", "RegionCode", "FROM_REGION_CODE", seqData.map(_.regionCode).toSet),
      ("country", "Country", "FROM_COUNTRY", seqData.map(_.country).toSet),
      ("region", "Region", "FROM_REGION", seqData.map(_.region).toSet),
      ("countryCode", "CountryCode", "ETHNICITY", seqData.map(_.countryCode).toSet)
    )

  def loadResource(name: String): String =
    val src = Source.fromResource(name)
    val res = src.getLines().mkString("") // not keeping new line characters
    src.close()
    res

  def between(left: String, right: String): Regex = (s"$left([^\"]*)$right").r

  def processSequence(seq: String): String =
    val len = if (List(312, 318) contains seq.length) 312 else 377
    val padding = if (len == 312) "N".repeat(27) else ""
    padding + seq.filterNot(_ == 'N')
                 .substring(0, len min seq.length)

  def removeUnnecessarySeq(seqs: Array[MtDNASequence]): Array[MtDNASequence] =
    seqs.filterNot(List("KT262553.1", "KT262558.1") contains _.id)

  def processRegionCode(isolate: String): String =
    "[A-Za-z]+".r.findFirstIn(isolate).get match {
      case "KSTRG" => "KSTR"
      case "SMLS" => "SML"
      case x => x
    }
}

case class FormatInfo(sep: String, getRegex: Map[String, Regex]) {
  def find(attr: String, data: String): String =
    getRegex(attr).unapplySeq(data) match
      case Some(matchedGroups) => matchedGroups.head
      case None => ""
}