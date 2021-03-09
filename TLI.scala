import scala.collection.mutable.Map
import scala.io.Source
import scala.io.StdIn.readLine

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
case class UnOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt

object TLI {
    private var counter:Int = 1
    private var SymbolTable: Map[String, Double] = Map[String, Double]()
    def eval(expr: Expr, symTable: Map[String, Double]): Double = expr match {
        case BinOp("*",e1,e2) => eval(e1,symTable) * eval(e2,symTable) 
        case BinOp("/",e1,e2) => eval(e1,symTable) / eval(e2,symTable) 
        case BinOp("+",e1,e2) => eval(e1,symTable) + eval(e2,symTable) 
        case BinOp("-",e1,e2) => eval(e1,symTable) - eval(e2,symTable) 
        case UnOp("==",e1,e2) => if(eval(e1,symTable) == eval(e2,symTable)) 1; else 0
        case BinOp("!=",e1,e2) => if(eval(e1,symTable) != eval(e2,symTable)) 1; else 0
        case BinOp(">=",e1,e2) => if(eval(e1,symTable) >= eval(e2,symTable)) 1; else 0
        case BinOp("<=",e1,e2) => if(eval(e1,symTable) <= eval(e2,symTable)) 1; else 0
        case BinOp(">",e1,e2) => if(eval(e1,symTable) > eval(e2,symTable)) 1; else 0
        case BinOp("<",e1,e2) => if(eval(e1,symTable) > eval(e2,symTable)) 1; else 0
        case Var(name) => symTable(name) //TLI.lookupVar(v: String, st: Map[String, Double])
        case Constant(num) => num
        case _ => println("eval error");System.exit(0); 0
    }
    def main(args: Array[String]):Unit = {
        //parse the file using the parser singlton object method parse which returns the symbol table as a map
        if(args(0).isEmpty()){
            println("no input file included");System.exit(0)
        }
        val stmtAndSymTable = Parser.run(args(0))
        val stmtMap = stmtAndSymTable._1
        SymbolTable = stmtAndSymTable._2
        while(counter <= stmtMap.size){
            TLI.perform(SymbolTable,stmtMap(counter))
            counter+= 1
        }
    }
    def perform(symTable: Map[String, Double],stmt: Stmt): Unit = stmt match {
        case Let(name,expr) => SymbolTable(name) = TLI.eval(expr,SymbolTable)
        case If(expr,label) if TLI.eval(expr,symTable) != 0 => counter = symTable(label).toInt
        case Input(label) => TLI.input(label)
        case Print(exprList) => print(TLI.printList(exprList, symTable))                  
        case _ => 
    }
    def input(label: String) = {
        val input = readLine()
        if (input.forall(y => y > 47 && y < 58)) {
            SymbolTable(label) = input.toDouble
        }
        else {
            
        }
    }
    def printList(eList: List[Expr], symTable: Map[String, Double]): String = eList match {
        case List() => "\n"
        case Str(s)::rest => (s + " " + TLI.printList(rest, symTable))
        case e::rest => TLI.eval(e,symTable) + " " + TLI.printList(rest, symTable)
        case _ => ""
    }
  }
/*
You will once again write a TL interpreter.
 This time in Scala. 
 The requirements are the same as program 3 with the modification that your program 
 should be based on the two abstract classes and related case classes provided in TLI.scala. 
 You will submit just one file, TLI.scala and the command "scala TLI testFile.txt" should run your program. */

class Parser  {
    private val symTable = Map[String,Double]()
    def parseStmt(words: List[String],lineNum: Int): Stmt = words match {
      case "let"::v::"="::expr if this.isValidName(v) => symTable += (v -> 0.0); Let(v,this.parseExpr(expr,lineNum))
      case "input"::v::Nil if this.isValidName(v) => symTable+= (v -> (0.0)); Input(v)
      case "print"::v => Print(this.parseExprList(v,lineNum));
      case "if"::rest => If(parseExpr(rest.takeWhile(_ != "goto"),lineNum),rest.dropWhile(_ != "goto").tail.head)
      case _ => println(words);print("Parse Statement error on line " + lineNum.toString + ".");
                System.exit(0); Input("E")
    }
    def parseExpr(words: List[String],lineNum: Int): Expr = words match {
        case e1::"*"::e2 => BinOp("*",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"/"::e2 => BinOp("/",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"+"::e2 => BinOp("+",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"-"::e2 => BinOp("-",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"<"::e2 => UnOp("<",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::">"::e2 => UnOp(">",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::">="::e2 => UnOp(">=",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"<="::e2 => UnOp("<=",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"=="::e2 => UnOp("==",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case e1::"!="::e2 => UnOp("!=",this.parseExpr(List(e1),lineNum),this.parseExpr(e2, lineNum))
        case x::rest if x.forall(y => {y > 47 && y < 58}) => Constant(x.toDouble)
        case x::rest if x.head > 64 && x.head < 123 && symTable.contains(x) => Var(x)
        case x::rest if x.head == '"' && x.last=='"' => Str(x.init.tail)   //for one word strings
        case x::rest if x.head == '"' && rest.last.last =='"' => Str((x::rest).toString.init.tail)  //for multiple word strings
        case _ => println(words);println("Parse Expr error on line " + lineNum.toString + ".");
                  System.exit(0); Var("Error")
    }
    def isValidName(name: String): Boolean = {
        true
    }
    def parseExprList(eList: List[String],lineNum: Int): List[Expr] = eList match {
        case List() => List()
        case list => val splitStringByComma = list.mkString(" ").split(" , ").toList;
                    println(splitStringByComma)
                     val exprStringList = splitStringByComma.map((x:String) => 
                        if(x.head == '"'){} 
                        else{x.split(" ").toList;}
                     println(exprStringList)
                     exprStringList.map(e => parseExpr(e, lineNum))
    }
}
object Parser { 
    private val stmtMap = Map[Int,Stmt]()
    def run(input:String) = {
        var lineNum: Int = 1
        val st = new Parser
        val file = Source.fromFile(input).getLines().filter(!_.isEmpty())
        for (line <- file){
            val tempLine = line.replaceAll("\t"," ")
            val words = Parser.rmPrecSpaces(tempLine).split(" ")
            val wordsList = words.toList
            if(wordsList.head.endsWith(":")){
                st.symTable += (wordsList.head.init -> lineNum)
                stmtMap += (lineNum -> st.parseStmt(wordsList.tail,lineNum))
            }else{
                stmtMap += (lineNum -> st.parseStmt(wordsList,lineNum))
            }
            lineNum += 1
        }
        (stmtMap,st.symTable)
    }
    def rmPrecSpaces(line: String): String = line match  {
        case x if x == "\n" => ""
        case x if x.head == ' ' => Parser.rmPrecSpaces(x.tail)
        case _ => line
    }
    /*def parseStmt(words: Array[String], lineNum: Int) = words match {
        //case  words(0) == "print" => Print(new List[Expr])
        case "let"::v::"="::expr if Parser.isValidName => Let(keyword,this.parseExpr(expr))
        //case _ if words(0) == "let" => Let(words(1),this.parseExpr(words.slice(1,words.size)))
        case _ => 
    }
    def ParseExpr(words: Array[String]) = words {
        
    }
    def isValidName*/
}