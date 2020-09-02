import java.io.FileWriter

/** Extracts Recipe Names and Ingredients from the Cookbook.
 *
 * Output file consists of desired Recipe Names with Ingredients after each recipe.
 */
object CookBook extends App {
  val workingDir = System.getProperty("user.dir")
  val srcName = if(args.nonEmpty) args(0) else s"$workingDir\\resources\\13177-8.txt"
  val dstName = s"$workingDir\\resources\\13177-8-results.txt"

  /** Reads a text file from program arguments or a default source, if arguments are empty */
  def openSource(fName: String): Seq[String] = {
      println(s"Reading a file from source: $fName")
      val filePointer = scala.io.Source.fromFile(fName)
      val myLines = filePointer.getLines.toList
      filePointer.close()

      myLines
  }

  /** Filters all the necessary information */
  def processSeq (mySeq: Seq[String]): Seq[String] = {
    //filters all non-empty lines that have a desired Recipe Name or Ingredients formatting style
    val tempList = mySeq.filter(el => !el.isEmpty && el(0).isUpper && el(1).isUpper || el.startsWith("    "))

    //excludes lines that have a desired style but do not belong to Recipes
    val tempList2 = tempList.toList.takeWhile(!_.equals("ESTABLISHED 1780"))

    //excludes unnecessary Recipe Names (ones that have a different Ingredients formatting)
    val tempList3 = for (List(el1, el2) <- tempList2.sliding(2) if el1(1).isUpper && !el2(1).isUpper || el1.startsWith("    "))
      yield el1

    //adds an empty line before and after the Recipe Name and different Ingredient parts (ex.: 1st BATCH, 2nd BATCH)
    val processedSeq = tempList3.toList.map(el => if (el.trim.charAt(1).isUpper || el.trim.last.isUpper) "\n" + el + "\n"  else el)

    processedSeq
  }

  /** Saves filtered sequence as an output file to the chosen destination  */
  def saveSeq(destName: String, mySeq: Seq[String]): Unit = {
    println(s"Saving a processed Cookbook to file $destName")
    val fw = new FileWriter(destName, true)
    mySeq.map(_ + "\n").foreach(fw.write)
    fw.close()
    println(s"All done processing the file $srcName into $destName.")
  }

  val mySeq = openSource(srcName)
  val filteredSeq = processSeq(mySeq)
  saveSeq(dstName, filteredSeq)
}
