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

        if (rightmostDerivation(tokens) == 0){
          printParseTree(tokens)
        }

      }
    }
  }
}



  def rightmostDerivation(tokens: Array[String]): Int ={
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


        var start = tokens.length - 2
        var lastFlag = flagChar(tokens,';',start)

        stringToPrint = stringToPrint.reverse
        ctr = 0

        if(lastFlag == -1){
          lastFlag = 0
        }

        //start bar a0, 0, axis a0, edge a0,a0, fill a0 stop

        var barReversed = "bar <x><y>,<y>".reverse
        var edgeReversed = "edge <x><y>,<x><y>".reverse
        var plotReversed = "<plot>".reverse
        var axisReversed = "axis <x><y>".reverse
        var fillReversed = "fill <x><y>".reverse

        var error = false
      breakable {
        while (true) {
          var checkNext = lastFlag
          checkNext += 1

          if (tokens(checkNext) == "bar") {
            stringToPrint = stringToPrint.replaceFirst(plotReversed, barReversed)
            println(stringToPrint.reverse)

            var barTokens: Array[String] = tokens.slice(checkNext, start + 1)
            stringToPrint = checkStatement(barTokens,"bar",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
          else if (tokens(checkNext) == "edge") {
            stringToPrint = stringToPrint.replaceFirst(plotReversed, edgeReversed)
            println(stringToPrint.reverse)

            var edgeTokens = tokens.slice(checkNext, start + 1)

            stringToPrint = checkStatement(edgeTokens,"edge",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }

          else if (tokens(checkNext) == "axis") {
            stringToPrint = stringToPrint.replaceFirst(plotReversed, axisReversed)
            println(stringToPrint.reverse)

            var axisTokens = tokens.slice(checkNext, start + 1)

            stringToPrint = checkStatement(axisTokens,"axis",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
          else if (tokens(checkNext) == "fill") {
            stringToPrint = stringToPrint.replaceFirst(plotReversed, fillReversed)
            println(stringToPrint.reverse)

            var fillTokens = tokens.slice(checkNext, start + 1)

            stringToPrint = checkStatement(fillTokens,"fill",stringToPrint);
            if (stringToPrint == "") {
              println("Statement does not follow grammar")
              error = true
              break
            }
          }
          else {
            println(tokens(checkNext) + " is not a valid plot")
            error = true
            break
          }

          if (ctr == 1) {
            break
          }

          if (lastFlag != 0) {
            start = lastFlag - 1
          }

          lastFlag = flagChar(tokens, ';', start)

          if (lastFlag == -1) {
            lastFlag = 0
            ctr += 1
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

      while(i!=0) {
        if (tokens(i) == flag.toString) {
          return i
        }
        i -= 1
      }
   -1
  }

  def checkStatement(tokens: Array[String], statementType: String, stringToPrint: String):String = {
    var start = tokens.length-1
    var newPrint = stringToPrint
    if(statementType == "bar"){
      if(tokens.length == 4){
        while(start>0){
          if (start == 3) {
            tokens(start) match {
              case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                newPrint = newPrint.replaceFirst(">y<",tokens(start))
                println(newPrint.reverse)
              }
              case _ => {
                println(tokens(start) + " is not a valid y")
                return ""
              }
            }
          }
          else if(start == 2){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 1){
            tokens(start)(1) match {
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst(">y<",tokens(start)(1).toString)
                println(newPrint.reverse)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }
            tokens(start)(0) match {
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst(">x<",tokens(start)(0).toString)
                println(newPrint.reverse)
              }
              case _ =>{
                  println(tokens(start)(0).toString +" is not a valid x")
                  return ""
                }
            }
          }
          start-=1
        }
      }
      else{
        if(tokens.length < 4) {
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
      if(tokens.length == 4){
        while(start>0){
          if (start == 3){
            tokens(start)(1) match {
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst(">y<",tokens(start)(1).toString)
                println(newPrint.reverse)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }
            tokens(start)(0) match {
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst(">x<",tokens(start)(0).toString)
                println(newPrint.reverse)
              }
               case _ =>{
                  println(tokens(start)(0).toString +" is not a valid x")
                  return ""
                }
            }
          }
          else if(start == 2){
            if(tokens(start) != ","){
              println(tokens(start)+" is not a comma")
              return ""
            }
          }
          else if(start == 1){
            tokens(start)(1) match {
              case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
                newPrint = newPrint.replaceFirst(">y<",tokens(start)(1).toString)
                println(newPrint.reverse)
              }
               case _ =>{
                  println(tokens(start)(1).toString +" is not a valid y")
                  return ""
                }
            }
            tokens(start)(0) match {
              case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
                newPrint = newPrint.replaceFirst(">x<",tokens(start)(0).toString)
                println(newPrint.reverse)
              }
               case _ =>{
                  println(tokens(start)(0).toString +" is not a valid x")
                  return ""
                }
            }
          }
          start-=1
        }
      }
      else{
        if(tokens.length < 4){
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
      if (tokens.length == 2) {
        tokens(start)(1) match {
          case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
            newPrint = newPrint.replaceFirst(">y<",tokens(start)(1).toString)
            println(newPrint.reverse)
          }
           case _ =>{
              println(tokens(start)(1).toString +" is not a valid y")
              return ""
            }
        }
        tokens(start)(0) match {
          case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
            newPrint = newPrint.replaceFirst(">x<",tokens(start)(0).toString)
            println(newPrint.reverse)
          }
          case _ =>{
              println(tokens(start)(0).toString +" is not a valid x")
              return ""
            }
        }
      }
      else{
        if(tokens.length < 2){
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
      if (tokens.length == 2) {
        tokens(start)(1) match {
          case '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' => {
            newPrint = newPrint.replaceFirst(">y<",tokens(start)(1).toString)
            println(newPrint.reverse)
          }
           case _ =>{
              println(tokens(start)(1).toString +" is not a valid y")
              return ""
            }
        }
        tokens(start)(0) match {
          case 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j' => {
            newPrint = newPrint.replaceFirst(">x<",tokens(start)(0).toString)
            println(newPrint.reverse)
          }
           case _ =>{
              println(tokens(start)(0).toString +" is not a valid x")
              return ""
            }
        }
      }
      else{
        if(tokens.length < 2){
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
