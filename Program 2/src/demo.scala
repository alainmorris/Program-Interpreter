import scala.util.control.Breaks._
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
    while(true) {
      println("\n\n\n<graph>→ start <plot_data> stop\n<plot_data>→ <plot>\n| <plot> , <plot_data >\n<plot>→ bar <x><y>,<y>\n| edge <x><y>,<x><y>\n| axis <x><y>\n| fill <x><y>\n<x>→ a | b | c | d | e | f | g | h | i | j\n<y>→ 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9")
      var userInput = scala.io.StdIn.readLine()

      if (userInput == "STOP") {
        break
      }
      else{

        var userInputv2 = userInput.replace(",","~,~");
        var userInputv3 = userInputv2.toString
        var userInputv4 = userInputv3.replace(";","~;~");
        var tokens = userInputv4.split("[~ ]+")

        if (leftmostDerivation(tokens) == 0){
          printParseTree(tokens)
        }

      }
    }
  }
}



  def leftmostDerivation(tokens: Array[String]): Int ={
      var stringToPrint: String = ""
      if(tokens(tokens.length - 1)!="stop"){
        var symbol: String = tokens(tokens.length - 1)
        stringToPrint = "Error: Instruction" + symbol + " is not valid"
        println(stringToPrint)
        return -1
      }
      else if(tokens(0)!="start"){
        var symbol: String = tokens(0)
        stringToPrint = "Error: Instruction "+symbol+" is not valid"
        println(stringToPrint)
        return -1
      }
    else{
        stringToPrint = "<graph> -> start <plot_data> stop"
        println(stringToPrint)

      var ctr = tokens.length-1
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

        var error = false
      breakable {
        while (true) {
//          var checkNext = lastFlag
//          checkNext -= 1

          if (tokens(start) == "bar") {
            stringToPrint = stringToPrint.replaceFirst(plot, bar)
            println(stringToPrint)

            var barTokens: Array[String] = tokens.slice(start+1, lastFlag)
            stringToPrint = checkStatement(barTokens,"bar",stringToPrint)
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
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
          else {
            println(tokens(start) + " is not a valid plot")
            error = true
            break
          }

          if(lastFlag == tokens.length-1){
            break
          }
          else{
            start = lastFlag + 1
          }

          lastFlag = flagChar(tokens, ';', start)

          if(lastFlag == -1){
            lastFlag = tokens.length - 1;
          }
        }
      }
        if(error == false){
          println("follows grammar")
        }



      }
    0
  }

  def printParseTree(tokens: Array[String]): Unit ={

  }

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

  def checkStatement(tokens: Array[String], statementType: String, stringToPrint: String):String = {
    var start = 0
    var newPrint = stringToPrint
    if(statementType == "bar"){
      if(tokens.length == 3){
        while(start<tokens.length){
          if (start == 2) {
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
          else if(start == 1){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 0){
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
        if(tokens.length < 3) {
          println("Not enough coordinates to plot bar")
          return ""
        }
        else{
          println("Too many coordinates to plot bar")
          return ""
        }
      }
    }
    else if(statementType == "edge"){
      if(tokens.length == 3){
        while(start<tokens.length){
          if (start == 2){
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
          else if(start == 1){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 0){
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
        if(tokens.length < 3){
          println("Not enough coordinates to plot edge")
          return ""
        }
        else{
          println("Too many coordinates to plot edge")
          return ""
        }
      }
    }
    else if(statementType == "axis") {
      if (tokens.length == 1) {
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
      else{
        if(tokens.length < 1){
          println("Not enough coordinates to plot axis")
          return ""
        }
        else{
          println("Too many coordinates to plot axis")
          return ""
        }
      }
    }
    else if(statementType == "fill") {
      if (tokens.length == 1) {
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
        if(tokens.length < 1){
          println("Not enough coordinates to plot axis")
          return ""
        }
        else{
          println("Too many coordinates to plot axis")
          return ""
        }
      }
    }
    newPrint
  }



}
