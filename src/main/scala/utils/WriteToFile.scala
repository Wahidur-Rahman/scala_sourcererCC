package utils

import java.io.{File, PrintWriter}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class WriteToFile {
  def writeToFile(filename:String, string:String):Unit={
    val writer =new PrintWriter(new File(filename))
    writer.write(string)
    writer.close()
  }

  def readFromFile(filename:String):ArrayBuffer[String] = {
    val result = new ArrayBuffer[String]()
    for (line <- Source.fromFile(filename).getLines) {
      result.append(line)
    }
    result
  }
}
