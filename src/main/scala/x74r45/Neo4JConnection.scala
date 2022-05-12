package x74r45

import org.neo4j.driver.{AuthTokens, Driver, GraphDatabase, Transaction}

class Neo4JConnection(uri: String, username: String, password: String) {
  private val driver: Driver = GraphDatabase.driver(uri, AuthTokens.basic(username, password))

  def close(): Unit = driver.close()

  def executeQuery(query: String): Unit =
    val session = driver.session()
    try {
      val result = session.writeTransaction(tx => tx.run(query))
      println(result)
    } finally if (session != null) session.close()
}
