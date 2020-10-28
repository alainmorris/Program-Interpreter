import scala.util.control.Breaks._
import processing.core.PApplet
import PGui.ProcessingTest
object demo {
  /*
  Step 1: Print BNF Grammar:
  Step 2: Loop for input
  Step 3: Tokenize input
  Step 4: Check for initial grammar rules
  Step 5: Perform rightmost derivation
  Step 6: Print parse tree
   */
def main(args: Array[String]): Unit = {

  breakable{
    //while loop that will run until user enters "STOP"
    while(true) {
      //Printing grammar
      println("\nLANGUAGE GRAMMAR:\n")
      println("    <graph>-> start <plot_data> stop\n<plot_data>->  <plot>\n             | <plot> , <plot_data >\n     <plot>->  bar <x><y>,<y>\n             | edge <x><y>,<x><y>\n             | axis <x><y>\n             | fill <x><y>\n        <x>->  a | b | c | d | e | f | g | h | i | j\n        <y>->  0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9\n")

      //reading input from user
      var userInput = scala.io.StdIn.readLine("(Enter Input or enter \"STOP\" to end program)> ")

      if (userInput == "STOP") {
        break
      }
      else{

        //tokenizing string
        var userInputv2 = userInput.replace(",","~,~");
        var userInputv3 = userInputv2.toString
        var userInputv4 = userInputv3.replace(";","~;~");
        var tokens = userInputv4.split("[~ ]+")
        

        //checking if leftmostderivation was successful, if yes we print tree and render the graphic
        if (leftmostDerivation(tokens)){
          printParseTree(tokens)
          renderGraphic(tokens)
        }

      }
    }
  }
}


//function that will perform leftmost derivation
  def leftmostDerivation(tokens: Array[String]): Boolean ={
    //initializing variables
      var error = false
      var stringToPrint: String = ""

    //doing initial check to see if last token is stop
      if(tokens(tokens.length - 1)!="stop"){
        var symbol: String = tokens(tokens.length - 1)
        stringToPrint = "Error: Instruction" + symbol + " is not valid"
        println(stringToPrint)
        return false
      }
        //doing initial check to see if first token is start
      else if(tokens(0)!="start"){
        var symbol: String = tokens(0)
        stringToPrint = "Error: Instruction "+symbol+" is not valid"
        println(stringToPrint)
        return false
      }
        //if we passed the intial checks then we derive.
    else{
        //printing initial derivation
        stringToPrint = "<graph> -> start <plot_data> stop"
        println(stringToPrint)

      var ctr = tokens.length-1

        //while loop that derives the plot_data 's
        while(ctr!=0){
          if (tokens(ctr) == ";"){
            stringToPrint = stringToPrint.replace("<plot_data>","<plot> ; <plot_data>");
            println(stringToPrint)
          }
          ctr-=1
        }

        stringToPrint = stringToPrint.replace("<plot_data>", "<plot>");
        println(stringToPrint)


        var start = 1
        var lastFlag = flagChar(tokens,';',start)


        ctr = 0

        if(lastFlag == -1){
          lastFlag = tokens.length-1
        }

        //start bar a0, 0, axis a0, edge a0,a0, fill a0 stop

        var bar = "bar <x><y>,<y>"
        var edge = "edge <x><y>,<x><y>"
        var plot = "<plot>"
        var axis = "axis <x><y>"
        var fill = "fill <x><y>"



      breakable {
        //while look that will run until code encounters a break
        while (true) {
//          var checkNext = lastFlag
//          checkNext -= 1

          //if the first plot_data is a bar then we run this code
          if (tokens(start) == "bar") {
            stringToPrint = stringToPrint.replaceFirst(plot, bar)
            println(stringToPrint)

            //get the tokens for this graph
            var barTokens: Array[String] = tokens.slice(start+1, lastFlag)
            //check the plot to see if it follows grammar.
            stringToPrint = checkStatement(barTokens,"bar",stringToPrint)

            //if it doesn't follow grammar string to print will come back as an empty string.
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
            //if it's an edge plot_data then we run this which is similar logic to bar
          else if (tokens(start) == "edge") {
            stringToPrint = stringToPrint.replaceFirst(plot, edge)
            println(stringToPrint)

            var edgeTokens = tokens.slice(start+1, lastFlag)

            stringToPrint = checkStatement(edgeTokens,"edge",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }

            //if it's an axis plot_data then we run this which is similar logic to bar
          else if (tokens(start) == "axis") {
            stringToPrint = stringToPrint.replaceFirst(plot, axis)
            println(stringToPrint)

            var axisTokens = tokens.slice(start+1, lastFlag)

            stringToPrint = checkStatement(axisTokens,"axis",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
            // if it's a fill plot_data then we run this which is similar logic to bar
          else if (tokens(start) == "fill") {
            stringToPrint = stringToPrint.replaceFirst(plot, fill)
            println(stringToPrint)

            var fillTokens = tokens.slice(start+1, lastFlag)

            stringToPrint = checkStatement(fillTokens,"fill",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
            //if the plot_data didn't begin with any of the tokens checked above then it wasn't a valid plot.
          else {
            println(tokens(start) + " is not a valid plot")
            error = true
            break
          }

          //if we reach the end of the derivation we break.
          if(lastFlag == tokens.length-1){
            break
          }
            //else the place we start from next is after the semicolon.
          else{
            start = lastFlag + 1
          }

          //lastflag will be the next semicolon
          lastFlag = flagChar(tokens, ';', start)

          //if there wasn't a semicolon then it will be the end of the tokens
          if(lastFlag == -1){
            lastFlag = tokens.length - 1;
          }
        }
      }
        //if there were no errors then we print this to show that it followed the grammar.
        if(error == false){
          println("follows grammar")
        }



      }
    !(error)
  }

  def printParseTree(tokens: Array[String]): Unit ={

  }

  //function to flag the position of the last "flag"
  def flagChar(tokens: Array[String], flag: Char, start: Int): Int ={
    var i = start

      while(i!=tokens.length) {
        if (tokens(i) == flag.toString) {
          return i
        }
        i += 1
      }
   -1
  }

  //function to check the validity of a statement to see if it follows the grammar of the language
  def checkStatement(tokens: Array[String], statementType: String, stringToPrint: String):String = {
    var start = 0
    var newPrint = stringToPrint
    //checking if the type of plot is a bar
    if(statementType == "bar"){
      //performing initial check to see if the tokens supplied do not exceed or not meet the requirement of length 3.
      if(tokens.length == 3){
        //while loop that runs while start is less than tokens.length
        while(start<tokens.length){
          if (start == 2) {
            //checking for a digit
            tokens(start) match {
              case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                newPrint = newPrint.replaceFirst("<y>",tokens(start))
                println(newPrint)
              }
              case _ => {
                println(tokens(start) + " is not a valid y")
                return ""
              }
            }
          }
            //checking for a comma
          else if(start == 1){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 0){
            //checking for a letter from a-j
            tokens(start)(0) match {
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst("<x>",tokens(start)(0).toString)
                println(newPrint)
              }
              case _ =>{
                println(tokens(start)(0).toString +" is not a valid x")
                return ""
              }
            }
            tokens(start)(1) match {
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst("<y>",tokens(start)(1).toString)
                println(newPrint)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }

          }
          start+=1
        }
      }
      else{
        //if the length of the tokens was less than 3 then we print this
        if(tokens.length < 3) {
          println("Not enough coordinates to plot bar")
          return ""
        }
          //if it's greater than 3 then we print this.
        else{
          println("Too many coordinates to plot bar")
          return ""
        }
      }
    }
     //if plot is an edge we run this block of code.
    else if(statementType == "edge"){
      //checking if the tokens.length is equal to 3
      if(tokens.length == 3){
        while(start<tokens.length){
          if (start == 2){
            //if start == 2 we try to match the token's first char to a letter from a-j to meet the grammar requirements.
            tokens(start)(0) match {
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst("<x>",tokens(start)(0).toString)
                println(newPrint)
              }
              case _ =>{
                println(tokens(start)(0).toString +" is not a valid x")
                return ""
              }
            }
            //we match the tokens second char to a digit
            tokens(start)(1) match {
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst("<y>",tokens(start)(1).toString)
                println(newPrint)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }

          }
            //checking for a comma
          else if(start == 1){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 0){
            tokens(start)(0) match {
                //matching token's first char to a letter from a-j
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst("<x>",tokens(start)(0).toString)
                println(newPrint)
              }
              case _ =>{
                println(tokens(start)(0).toString +" is not a valid x")
                return ""
              }
            }
            tokens(start)(1) match {
                //matching token's second char to a digit
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst("<y>",tokens(start)(1).toString)
                println(newPrint)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }

          }
          start+=1
        }
      }
      else{
        //if the length of tokens is less than 3 we print this error
        if(tokens.length < 3){
          println("Not enough coordinates to plot edge")
          return ""
        }
          //if the length of tokens exceeds 3 we print this
        else{
          println("Too many coordinates to plot edge")
          return ""
        }
      }
    }
      //if the plot is an axis we run this block of code.
    else if(statementType == "axis") {
      if (tokens.length == 1) {
        //match token's first character to a letter from a-j to satisfy grammar
        tokens(start)(0) match {
          case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
            newPrint = newPrint.replaceFirst("<x>",tokens(start)(0).toString)
            println(newPrint)
          }
          case _ =>{
            println(tokens(start)(0).toString +" is not a valid x")
            return ""
          }
        }
        //match token's second character to a digit to satisfy grammar.
        tokens(start)(1) match {
          case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
            newPrint = newPrint.replaceFirst("<y>",tokens(start)(1).toString)
            println(newPrint)
          }
           case _ =>{
              println(tokens(start)(1).toString +" is not a valid y")
              return ""
            }
        }

      }
      else{
        //if the length is less than 1 we print this error
        if(tokens.length < 1){
          println("Not enough coordinates to plot axis")
          return ""
        }
          //if the length is greater than 1 we print this error.
        else{
          println("Too many coordinates to plot axis")
          return ""
        }
      }
    }
      //if the plot is a fill we run this block of code.
    else if(statementType == "fill") {
      if (tokens.length == 1) {
        //match token's first character to a letter from a-j to satisfy grammar.
        tokens(start)(0) match {
          case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
            newPrint = newPrint.replaceFirst("<x>", tokens(start)(0).toString)
            println(newPrint)
          }
          case _ =>{
            println(tokens(start)(0).toString +" is not a valid x")
            return ""
          }
        }
        //match token's second character to a digit to satisfy grammar.
        tokens(start)(1) match {
          case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
            newPrint = newPrint.replaceFirst("<y>",tokens(start)(1).toString)
            println(newPrint)
          }
           case _ =>{
              println(tokens(start)(1).toString +" is not a valid y")
              return ""
            }
        }

      }
      else{
        //if the length is less than 1 we print this error
        if(tokens.length < 1){
          println("Not enough coordinates to plot axis")
          return ""
        }
          //if the length is greater than 1 we print this error.
        else{
          println("Too many coordinates to plot axis")
          return ""
        }
      }
    }
    newPrint
  }


  def renderGraphic(tokens: Array[String]){
    println("\ntest graph\n")
    var filteredTokens = tokens.toList.filter(!Array("start", "stop",";",",").contains(_))
    println(filteredTokens)
    var pGui = new ProcessingTest(filteredTokens.toArray)

    PApplet.runSketch(Array("PGui.ProcessingTest"), pGui)
    scala.io.StdIn.readLine("Press Enter To Close Graph and Continue...")
    pGui.closeGUI()

  }
}