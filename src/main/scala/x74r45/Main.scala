package x74r45

object Main {
  val ImportRelationships: Boolean = false
  val UnifySequences: Boolean = true

  def main(args: Array[String]): Unit =
    val seqData = Parsing.parseData(List(("Ukraine", "Ukraine"), ("BaltoSlavic", "Balto_Slavic")))
    println("Data parsed!")
    Importing.importSequenceData(seqData)
    Importing.importBasicSequences()
    println("Data imported!")
    if (ImportRelationships)
      Importing.importGroupedAttributes(Parsing.groupAttributes(seqData))
      println("Relationships created!")
}
