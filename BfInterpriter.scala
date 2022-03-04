
  abstract class Node
  class Inst(val inst: Char) extends Node{
    override def toString(): String = "%c".format(inst)
  }
  class Block(val block: List[Node])extends Node{
    override def toString(): String = block.mkString("[","","]")
  }

object BfInterpriter {

  def makeNode(s: String,result: List[Node],isRoot: Boolean): (List[Node],String)={
    if(s == ""){
      if(isRoot) return (result,"")
      else throw new Exception("makeNode(block) => ']' not found!")
    }
    val (n:Node,tail:String) = (s.head) match {
      case '+' | '-' | '<' | '>' | '.' | ','=> 
        (new Inst(s.head), s.tail)
      case '[' => 
        val (n,rest) = makeNode(s.tail,Nil,false)
        (new Block(n.reverse), rest)
      case ']' =>
        if(isRoot) throw new Exception("makeNode(root) => too many ']'")
        else       return (result, s.tail)
      case _ => return makeNode(s.tail, result, isRoot)
    }
    return makeNode(tail, n::result, isRoot)
  }

  def parse(code: String): Block = {
    val (ns:List[Node], rest: String) = makeNode(code, Nil, true)
    new Block(ns.reverse)
  }

  class Evaluator{
    var ptr = 0
    val mem: Array[Int] = new Array(32)
    def eval(insts: List[Node]): Unit = {
      insts.head match {
        case a:Inst =>
          a.inst match {
            case '+' => mem(ptr)+=1
            case '-' => mem(ptr)-=1
            case '>' => ptr+=1
            case '<' => ptr-=1
            case '.' => printf("%c",mem(ptr))
            case ',' => mem(ptr) = System.in.read()
          }
        case x:Block =>
          while(mem(ptr) != 0){
            eval(x.block)
          }
      }
      if(insts.tail != Nil) eval(insts.tail)
    }
  }  

  def main(args: Array[String]) :Unit = {
    val src =
      if(args.length != 0) args(0)
      else 
        ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-]<.>+++++++++++[<+++++>-]<.>++++++++[<+++>-]<.+++.------.--------.[-]>++++++++[<++++>-]<+.[-]++++++++++."
    val node = parse(src)
    println(node.block.mkString("[","","]"))
    val e = new Evaluator
    e.eval(node.block)
    println()
  }
}