package utils

import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}

/*
because of the scala meta's own bug, the source code needs to be processed.
 */
class SortFile {
  def sortFile(filename:String):Unit={
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val lines=Source.fromFile(filename)(decoder).getLines().toList
    var isContain=false
    var string =""
    for(line<-lines){
      if(line.contains("s\"")){
        string = string +line+"\n "
        isContain=true
      }else{
        string = string +line+"\n"
      }
    }
    if(isContain){
      val w=new WriteToFile()
      w.writeToFile(filename,string)
    }
  }

}
