package x74r45

import x74r45.model.*

object Importing {
  def importSequenceData(data: Seq[MtDNASequence]): Unit =
    val query = s"CREATE ${data.map(_.toCypher).mkString(",")}"
    executeQuery(query)

  def importBasicSequences(): Unit =
    val query =
      s"""CREATE (Andrews:BasicSequence {
           |id: "rCRS",
           |name: "Andrews",
           |sequence: "TTCTTTCATGGGGAAGCAGATTTGGGTACCACCCAAGTATTGACTCACCCATCAACAACCGCTATGTATTTCGTACATTACTGCCAGCCACCATGAATATTGTACGGTACCATAAATACTTGACCACCTGTAGTACATAAAAACCCAATCCACATCAAAACCCCCTCCCCATGCTTACAAGCAAGTACAGCAATCAACCCTCAACTATCACACATCAACTGCAACTCCAAAGCCACCCCTCACCCACTAGGATACCAACAAACCTACCCACCCTTAACAGTACATAGTACATAAAGCCATTTACCGTACATAGCACATTACAGTCAAATCCCTTCTCGTCCCCATGGATGACCCCCCTCAGATAGGGGTCCCTTGAC"
         |}), (Eva:BasicSequence {
           |id: "RSRS",
           |name: "Eva",
           |sequence: "TTCTTTCATGGGGAAGCAGATTTGGGTACCACCCAAGTATTGACTCACCCATCAACAACCGCTATGTATTTCGTACATTACTGCCAGCCACCATGAATATTGTACAGTACCATAAATACTTGACCACCTGTAGTACATAAAAACCCAATCCACATCAAAACCCTCCCCCCATGCTTACAAGCAAGTACAGCAATCAACCTTCAACTGTCACACATCAACTGCAACTCCAAAGCCACCCCTCACCCACTAGGATATCAACAAACCTACCCACCCTTAACAGTACATAGCACATAAAGCCATTTACCGTACATAGCACATTACAGTCAAATCCCTTCTCGTCCCCATGGATGACCCCCCTCAGATAGGGGTCCCTTGAC"
         |})""".stripMargin
    executeQuery(query)

  def importGroupedAttributes(attrs: List[(String, String, String, Set[String])]): Unit =
    for { (attrName, className, relName, values) <- attrs
          value <- values } {
      executeQuery(s"CREATE (attr:$className {value: \"$value\"})")
      executeQuery(
        s"""MATCH (seq:Sequence), (attr:$className)
           |WHERE seq.$attrName = "$value" AND attr.value = "$value"
           |CREATE (seq)-[:$relName]->(attr)
           |""".stripMargin
      )
    }
    for ((attrName, _, _, _) <- attrs) {
      executeQuery(s"MATCH (seq:Sequence) REMOVE seq.$attrName")
    }

  def executeQuery(query: String): Unit =
    val connection = new Neo4JConnection("bolt://localhost:7687", "neo4j", "password")
    try connection.executeQuery(query)
    finally if (connection != null) connection.close()
}
