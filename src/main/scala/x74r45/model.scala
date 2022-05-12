package x74r45

object model {
  case class MtDNASequence(
    id: String,
    isolate: String,
    carriers: Int,
    haplogroup: String,
    haplogroupComment: String,
    db: String,
    regionCode: String,
    country: String,
    region: String,
    sequence: String
  ) {
    def length: Int = sequence.length

    def countryCode: String = 
      if (db == "Ukraine") "UKR" else MtDNASequence.countryCodes(country)

    def toCypher: String =
      s"""(:Sequence {
         |id: "$id",
         |isolate: "$isolate",
         |length: $length,
         |carriers: "$carriers",
         |haplogroup: "$haplogroup",
         |haplogroupComment: "$haplogroupComment",
         |db: "$db",
         |regionCode: "$regionCode",
         |country: "$country",
         |region: "$region",
         |countryCode: "$countryCode",
         |sequence: "$sequence"
         |})""".stripMargin
  }

  object MtDNASequence {
    val countryCodes: Map[String, String] = Map(
      "Russia" -> "RU",
      "Belarus" -> "BEL",
      "Czech Republic" -> "CZ",
      "Lithuania" -> "LT",
      "Ukraine" -> "UKR"
    )
  }
}