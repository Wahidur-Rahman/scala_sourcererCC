package visitors

import utils.FindOriginCode
import utils.WriteToFile
import java.io.File

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListMap
import scala.collection.mutable
object Scalaparser {

  var globalTokensFrequency = scala.collection.mutable.Map[String, Int]()
  var listGlobalFrequencyTokens = List[(String,Int)]()
  var sortedGlobalFrequencyTokens = scala.collection.mutable.Map[String, Int]()
  var method_tokens = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]]()//used for original counting method
  var sorted_method_tokens = scala.collection.mutable.Map[String, scala.collection.mutable.Map[String, Int]]()
  var method_map = scala.collection.mutable.Map[String,Array[String]]()
  var sorted_method_map = scala.collection.mutable.Map[String,Array[String]]()
  var partialIndex = scala.collection.mutable.Map[String,ArrayBuffer[(String,Int)]]()
  var cloneList = ""
  var uniqueTokens = scala.collection.Set[String]()
  var boundary = 0.7
  var cloneMap = scala.collection.mutable.Map[String,Double]()
  def main(strings: Array[String]): Unit = {
    val mb = 1024d *1024d
    val runtime = Runtime.getRuntime
    println("Allocated memory: " + runtime.freeMemory()/mb)
    println("Current memory usage: " + (runtime.totalMemory() - runtime.freeMemory())/mb)
    println("Free memory: " + runtime.freeMemory()/mb)
    val visitor = new Visitor()
    val w = new WriteToFile()
    val findOriginCode = new utils.FindOriginCode
    var args = ""
    var fileName = ""
    var ids = ""
    //fileName = strings(0)
    fileName = "/Users/rahmanw/Dev/Data Spartan/spark/graphx/"
    println("starting directory " + fileName)
    parse_file(new File(fileName), visitor)

    w.writeToFile("leaf.txt", visitor.identifierResult)
    w.writeToFile("path.txt", visitor.ASTResult)
    w.writeToFile("method.txt", visitor.methodName)

    var leaf = w.readFromFile("leaf.txt")
    var path = w.readFromFile("path.txt")
    var method = w.readFromFile("method.txt")
    println(method.length + "  methods")
    println("Combining leaf + path")
    //combine tokens from leaf and path
    for(i <- 0 until method.length){
      var tokens = leaf(i) + " " + path(i)
      var tokens_split = tokens.split(" ")
      val tokens_map = count_tokens(tokens_split)
      method_map(method(i)) = tokens_split
    }
    /*
    if (args.equals("-identifier")) {
      w.writeToFile("sample-training-file.txt", visitor.identifierResult)
    } else if (args.equals("-AST")) {
      w.writeToFile("sample-training-file.txt", visitor.ASTResult)
    } else if (args.equals("-find-source-code")) {
      findOriginCode.findOriginCode(ids, visitor.sourceMethodCode, visitor.sourceFileName) //Find the source code based on the result id
    }

    */

    //Ordering the Global Frequency Map so no two objects have the same rank.
    listGlobalFrequencyTokens = ListMap(globalTokensFrequency.toSeq.sortBy(_._2):_*).toList
    for(i <- 0 until listGlobalFrequencyTokens.size){
      var newTuple = (listGlobalFrequencyTokens(i)._1,i)
      sortedGlobalFrequencyTokens(listGlobalFrequencyTokens(i)._1) = i
    }

    println("sorting...")
    uniqueTokens = globalTokensFrequency.keySet

    //Sorting methods
    sortGlobalMethodMap()

    println("Calculating original score")
    val t0 = System.nanoTime()
    var counter = 0
/*
    var scores =
      (for {fl <- sorted_method_tokens;
            f2 <- sorted_method_tokens;

            if fl._1.compare(f2._1) < 0}
        yield (fl._1 + " " + f2._1, similarity_measure(fl._2, f2._2,fl._1,f2._1))
        ).toMap[String, Double]
    var scores_ordered = scores.toSeq.sortBy(-_._2)
    //println("Got here" + scores_ordered)
    for (p <- scores_ordered) {
      if (p._2 >= boundary) {
        println(p._1 + ": " + p._2)
        counter +=1
        //cloneList = cloneList + p._1 + " " + p._2 + "\n"
      }
    }
*/

    println("Writing results to file")


    val t2 = System.nanoTime()

    println("Creating partial Index")
    createPartialIndex(boundary)
    println("Detecting clones")
    detectClones(boundary)
    val t3 = System.nanoTime()
    for(key <- cloneMap.keySet){
      println(key + " - " + cloneMap(key))
      cloneList += key + " " + cloneMap(key) + "\n"
    }
    w.writeToFile("cloneList.txt",cloneList)
    println(cloneMap.size + " clones from SourcererCC style tool")
    println("Time (s): " + (t3-t2)/1000000000d)

  }


  def parse_file(file: File,p:Visitor): Unit = {

    if (file.isFile && file.getName.endsWith(".scala")) {
      p.temName=file.getAbsolutePath
      println(p.temName)
      p.parse(file.getAbsolutePath)
    } else if (file.isDirectory) {
      for (subFile <- file.listFiles()) {
        parse_file(subFile,p)
      }
    }

  }

  def count_tokens(token_list: Array[String]): scala.collection.mutable.Map[String, Int] = {
    var tokens = scala.collection.mutable.Map[String, Int]()
    for(token <- token_list){
      try {
        globalTokensFrequency(token) += 1
      }
      catch {
        case e: NoSuchElementException => globalTokensFrequency += (token -> 1)
      }
      try {
        tokens(token) += 1
      }
      catch {
        case e: NoSuchElementException => tokens += (token -> 1)
      }
    }

    tokens
  }
  def similarity_measure(m1: scala.collection.mutable.Map[String, Int], m2: scala.collection.mutable.Map[String, Int],f1:String,f2:String): Double = {
    val l1 = m1.foldLeft[Int](0)(_ + _._2)
    val l2 = m2.foldLeft[Int](0)(_ + _._2)
    val len = Math.max(l1, l2)

    val overlap = m1.keySet.intersect(m2.keySet).foldLeft(0)((r, k) => r + Math.min(m1(k), m2(k)))
    overlap.toDouble/len
  }

  def sortGlobalMethodMap():Unit = {
    for(method <- method_map.keySet){
      var sortedMethod = sort(method_map(method))
      var methodFreq =  sortedMethod.groupBy(identity).mapValues(_.size)
      sorted_method_tokens(method) = collection.mutable.Map(methodFreq.toSeq: _*)
      sorted_method_map(method) = sortedMethod
    }
  }

  def sort(method:Array[String]):Array[String] = {
    var sortedMethod = Array.ofDim[String](method.length)
    var globalFrequency = scala.collection.mutable.Map[String,Int]()

    //local frequency of items in current code block
    var localFrequency = method.groupBy(identity).mapValues(_.size)
    //We want the frequency of tokens in the current method with respect to the global frequency
    for(token <- localFrequency.keySet){
      try {
        globalFrequency(token) = sortedGlobalFrequencyTokens(token)
      }
      catch {
        case e: NoSuchElementException =>
          throw new IllegalArgumentException("This block should be possible to enter...")
      }
    }

    //order this global frequency map from smallest to largest.
    var sortedGlobalFrequency = ListMap(globalFrequency.toSeq.sortBy(_._2):_*)
    var i = 0

    //Now create the ordered method based on the global frequency
    for(token <- sortedGlobalFrequency.keySet){
      var methodFrequency = localFrequency(token)
      for(j <-0 until methodFrequency){
        sortedMethod(i + j) = token
      }
      i = i + methodFrequency
    }
    sortedMethod
  }

  def createPartialIndex(theta: Double):Unit = {
    //same nomenclature as sourcererCC paper for traceability
    //first populate index with unique tokens and empty lists
    var totalTokensToBeIndexed = 0
    for(token <- uniqueTokens){
      partialIndex(token) = new ArrayBuffer[(String, Int)]()
    }
    for(method <- sorted_method_map.keySet){
      //println(method)
      var b = sorted_method_map(method)
      var bLength = b.length
      var number = (bLength - math.ceil(theta.toDouble * bLength) + 1d)
      var tokensToBeIndexed = (bLength - math.ceil(theta.toDouble * bLength) + 1d).toInt
      //println("Tokens to be indexed:" + tokensToBeIndexed + " - " + bLength)
      if(tokensToBeIndexed > bLength){//Edge case at theta = 0
        tokensToBeIndexed = bLength
      }
      totalTokensToBeIndexed += bLength
      var t = ""
      for(i <- 0 until tokensToBeIndexed){
        t = b(i)
        partialIndex(t).append((method,i))

      }
    }
  }
  def detectClones(theta:Double):Unit = {
    //same nomenclature as sourcererCC paper for traceability
    for (method <- sorted_method_map.keySet){
      var b = sorted_method_map(method)

      var bLength = b.length
      var candSimMap = scala.collection.mutable.Map[String,(Int, Int,Int)]()
      //get frequency map from block.
      //var bFrequency = sorted_method_map(method)
      var number_of_matches = 0
      var locations_matched = ArrayBuffer[(String,Int)]()
      var querySubBlock = (bLength - math.ceil(theta.toDouble * bLength) + 1d).toInt
      if(querySubBlock > bLength){//Edge case at theta = 0
        querySubBlock = bLength
      }
      //var previousToken = "This is not a token" // To ensure we are not repeat counting the same token due to ordering
      var ct = 0
      for(i <- 0 until querySubBlock){
        var t = b(i)
        var methods_matched = ArrayBuffer[String]()
        var tokenLocs = partialIndex(t)
        for(location <- tokenLocs){
          if(!locations_matched.contains(location) && !methods_matched.contains(location._1)) {//ensuring we haven't already matched this token location for this block.
            locations_matched.append(location)
            //per iteration of a token we are allowed 1 max overlap per method
            var c_method = location._1
            methods_matched.append(c_method)
            var c = sorted_method_map(c_method)
            var cLength = c.length
            var j = location._2
            if (cLength > math.ceil(theta.toDouble * bLength).toInt && c_method != method) {
              ct = math.ceil(math.max(cLength, bLength) * theta.toDouble).toInt //amount of tokens we need to overlap
              var uBound = 1 + math.min(bLength - i, cLength - j)
              if (candSimMap.keySet.exists(_ == c_method)) {
                var tuples = candSimMap(c_method)
                var count = tuples._1
                if (count + uBound >= ct) {
                  var new_tuple = (count + 1, j,i)
                  number_of_matches += 1
                  candSimMap(c_method) = new_tuple
                } else {
                  candSimMap -= c_method //remove rather than set value to (0,0)
                }
              } else {
                //initialise with (0,0) and do the checks
                var count = 0
                candSimMap(c_method) = (0,0,0)
                if (count + uBound >= ct) {
                  var new_tuple = (count + 1, j,i)
                  number_of_matches += 1
                  candSimMap(c_method) = new_tuple
                } else {
                  candSimMap -= c_method //remove rather than set value to (0,0)
                }
              }
            }
          }
        }
      }
      verifyCandidates(method,candSimMap,theta)
    }
  }
  def verifyCandidates(b_method:String, candSimMap:scala.collection.mutable.Map[String,(Int,Int,Int)],theta:Double):Unit = {
    var b = sorted_method_map(b_method)
    for(c_method <- candSimMap.keySet){
      var cloneString1  = b_method + " " + c_method
      var cloneString2  = c_method + " " + b_method
      var keys = cloneMap.keySet
      if(!(keys.contains(cloneString1) || keys.contains(cloneString2))) {
        var c = sorted_method_map(c_method)
        var cLength = c.length
        var bLength = b.length
        var ct = math.ceil(math.max(cLength, bLength) * theta.toDouble).toInt //amount of tokens we need to overlap
        var c_tuple = candSimMap(c_method)
        var current_matches = c_tuple._1
        var total_matches = current_matches
        var remaining_matches = ct - current_matches //paper uses just ct in error. Hasn't accounted for current matches
        if (cLength > 0) {
          var tokPosc = c_tuple._2 + 1
          var tokPosb = c_tuple._3 + 1
          if (tokPosb > bLength) {
            //Edge case at theta = 0
            tokPosb = bLength
          }
          while(tokPosb < bLength && (tokPosc) < cLength){
            if(math.min(bLength - tokPosb,cLength - tokPosc) >= remaining_matches){
              if(b(tokPosb) == c(tokPosc)){
                var new_tuple = (c_tuple._1 + 1, (tokPosc),tokPosb)
                candSimMap(c_method) = new_tuple
                remaining_matches -= 1
                total_matches += 1
                tokPosb +=1
                tokPosc +=1
              }else{
                if(sortedGlobalFrequencyTokens(b(tokPosb)) < sortedGlobalFrequencyTokens(c(tokPosc))){
                  tokPosb +=1
                }else{
                  tokPosc +=1
                }
              }
            }else{
              //break loop by artificially setting tokPosb > bLength
              tokPosb += bLength
            }
          }
        }
        var similarity = (total_matches.toDouble/math.max(bLength,cLength))
        if(total_matches >= ct){
          cloneMap(cloneString1) = similarity
        }
      }
    }
  }
}
