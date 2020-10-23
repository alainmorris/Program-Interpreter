package PGui

import processing.core.PApplet
import processing.core.PConstants._
import scala.annotation.switch

class ProcessingTest(tokens: Array[String]) extends PApplet {
  val yMax = 600
  val yMin = 100
  val xMax = 550
  val xMin = 50
  var _isClosed = false
  def isClosed(): Boolean={
    _isClosed
  }
  override def settings(): Unit = {
      size(900, 700)
  }
  
  override def setup() {
      background(0)
      surface.setAlwaysOnTop(true);
  }
  override def draw() {
      frame.requestFocus()
      renderFromTokens()
  }
  def closeGUI(){
    exit()
    _isClosed = true
  }
  def renderFromTokens(){
    var mTokens = tokens
    var count = 0
    
    while(count < mTokens.length){
      var token = mTokens.toBuffer.remove(count)
      count += 1

      if(token == "axis"){
        var input = mTokens.toBuffer.remove(count)
        (new Axis(
          yMax, 
          xMax, 
          yMin, 
          xMin, 
          input.split("")(0), 
          input.split("")(1) 
        )).show()
        count += 1
      }
      else if(token=="bar"){

        var input = mTokens.toBuffer.remove(count)
        count += 1
        var width = mTokens.toBuffer.remove(count)
        count += 1
        (new Bar(
          yMax, 
          xMax, 
          yMin, 
          xMin, 
          input.split("")(0), 
          input.split("")(1), 
          width
        )).show()
      
      }
      else if(token == "edge"){
        var input1 = mTokens.toBuffer.remove(count)
        count+=1
        var input2 = mTokens.toBuffer.remove(count)
        count+=1
        (new Edge(
          yMax, 
          xMax, 
          yMin, 
          xMin, 
          input1.split("")(0), 
          input1.split("")(1), 
          input2.split("")(0), 
          input2.split("")(1)
        )).show()
      }
    }
  }
  override def keyPressed(){ 
    exit()
    _isClosed = true
  }
  class Edge(yMax:Int, xMax: Int, yMin: Int, xMin:Int, x1: String, y1:String, x2: String, y2:String){
    def show(){
      stroke(44, 203, 0)
      fill(255)
      var x11 = (xMin)+(('a' to x1(0)).length * 50)
      var x22 = (xMin)+(('a' to x2(0)).length * 50)
      var y11 = (yMax)-(y1.toInt * 50)
      var y22 = (yMax)-(y2.toInt * 50)
      line(x11, y11,x22,y22)
    }

  }
  class Bar(yMax:Int, xMax: Int, yMin: Int, xMin:Int, xInput: String, yInput:String, width: String){
    def show(){
      stroke(44, 203, 0)
      fill(0)
      var x1 = (xMin)+(('a' to xInput(0)).length * 50)
      var y1 = (yMax)-(yInput.toInt * 50)
      var yHieght = yInput.toInt * 50
      var xWidth = (width.toInt * 50)
      rect(x1, y1, xWidth, yHieght)
    }
  }
  class Axis(yMax:Int, xMax: Int, yMin: Int, xMin:Int, xInput: String, yInput:String){
    def show(){
      fill(255)
      drawHorizontalLine()
      addXFields()
      drawVerticalLine()
      addYFields()
    }
    def drawVerticalLine(){
      stroke(250)
      line(xMin, yMin + (yMax - (yInput.toInt+2)*50), xMin, yMax)
    }
    def drawHorizontalLine(){
      stroke(250)
      var xInputCharArr = ('a' to xInput(0))
      var xEnd = xMin + (xInputCharArr.length*50)
      line(xMin, yMax, xEnd, yMax)
    }
    def addYFields(){
      stroke(200)      
      var x = xMin - 15
      var y = yMax
      var count = 0 
      while(count < (yInput.toInt +1)){
        line(xMin, y, xMin -5, y)
        text(count, x, y)
        y -= 50
        count += 1
      }
    }
    def addXFields(){
      stroke(200)
      var x = xMin+50
      var y = yMax + 15
      var count = 0
      var xInputCharArr = ('a' to xInput(0))
      while(count < (xInputCharArr.length)){
        line(x, yMax + 5, x, yMax)
        text(xInputCharArr(count), x, y)
        x += 50
        count += 1
      }
    }
  }
}
