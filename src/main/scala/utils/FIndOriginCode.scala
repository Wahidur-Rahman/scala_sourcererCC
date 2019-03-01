package utils

import java.nio.charset.CodingErrorAction

import scala.collection.mutable.ArrayBuffer
import scala.io.{Codec, Source}

/**
  * find source code based on id
  */
class FindOriginCode {
  def findOriginCode(idsFile:String,strings:ArrayBuffer[String],fileNames:ArrayBuffer[String]):Unit={
    val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
    val lines=Source.fromFile(idsFile)(decoder).getLines().toList
    var index=1
    var string ="1. *********************************************************************\n"
    for(line<-lines){
      val ids = line.split(",")
      if(ids.length>1){
        string=string+"file:  "+fileNames(Integer.parseInt(ids(0)))+"\n\n"
        string=string+strings(Integer.parseInt(ids(0)))
        string=string +"\n\n"
        string=string+"file:  "+fileNames(Integer.parseInt(ids(1)))+"\n\n"
        string=string +strings(Integer.parseInt(ids(1)))
        string=string+"\n\n"
        index=index+1
        string=string+index+". **********************************************************************\n"
      }
    }
    val w=new WriteToFile()
    w.writeToFile("cloneResult.txt",string)
  }
}

