import scala.language.implicitConversions
import scala.language.reflectiveCalls

abstract class Token extends Serializable 
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token

// more convenience for the semantic actions later on
case class ~[+A, +B](_1: A, _2: B)

type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I) : Set[T] =
    for ((head, tail) <- parse(ts); if tail.isEmpty) yield head
}

class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield (new ~(head1, head2), tail2)
}

class AltParser[I : IsSeq, T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I : IsSeq, T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

case class StringParser(s: String) extends Parser[String, String] {
  def parse(sb: String) = {
    val (prefix, suffix) = sb.splitAt(s.length)
    if (prefix == s) Set((prefix, suffix)) else Set()
  }
}


case object NumParser extends Parser[String, Int] {
  val reg = "[0-9]+".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => {
      val (head, tail) = sb.splitAt(s.length)
      Set((head.toInt, tail)) 
    }
  }
}

NumParser.parse("1abc")
NumParser.parse_all("1")

case object NumParser extends Parser[List[Token], Int] {

  def parse(sb: List[Token]) = sb.lift(0) match {
    case None => Set()
    case Some(s) => {
      s.isInstanceOf[T_NUM] match {
        case false => Set()
        case true => {
          val (_, tail) = sb.splitAt(1)
          Set((s.asInstanceOf[T_NUM].n.toInt, tail))
        }
      }
    }
  }
}

// NumParser.parse(List())
// NumParser.parse(List(T_NUM(1), T_ID("abc")))
// NumParser.parse_all(List(T_NUM(1)))


implicit def string2parser(s : String) = StringParser(s)

implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}


implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

implicit def StringOps(s: String) = new {
  def || (q : => Parser[String, String]) = new AltParser[String, String](s, q)
  def || (r: String) = new AltParser[String, String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
  def ~[S](q : => Parser[String, S]) = 
    new SeqParser[String, String, S](s, q)
  def ~ (r: String) = 
    new SeqParser[String, String, String](s, r)
}


// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp 

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt


case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp


case object IdParser extends Parser[String, String] {
  val reg = "[a-z][a-z,0-9]*".r
  def parse(sb: String) = reg.findPrefixOf(sb) match {
    case None => Set()
    case Some(s) => Set(sb.splitAt(s.length))
  }
}


case object IdParser extends Parser[List[Token], String] {
   def parse(sb: List[Token]) = sb.lift(0) match {
    case None => Set()
    case Some(s) => {
      s.isInstanceOf[T_ID] match {
        case false => Set()
        case true => {
          val (_, tail) = sb.splitAt(1)
          Set((s.asInstanceOf[T_ID].s.toString, tail))
        }
      }
    }
  }
}



lazy val AExp: Parser[String, AExp] = 
  (Te ~ "+" ~ AExp) ==> { case x ~ _ ~ z => Aop("+", x, z): AExp } ||
  (Te ~ "-" ~ AExp) ==> { case x ~ _ ~ z => Aop("-", x, z): AExp } || Te 
lazy val Te: Parser[String, AExp] = 
  (Fa ~ "*" ~ Te) ==> { case x ~ _ ~ z => Aop("*", x, z): AExp } || 
  (Fa ~ "/" ~ Te) ==> { case x ~ _ ~ z => Aop("/", x, z): AExp } || Fa  
lazy val Fa: Parser[String, AExp] = 
  //  ("(" ~ AExp ~ ")") ==> { case _ ~ y ~ _ => y } || 
   IdParser ==> Var || 
   NumParser ==> Num





// boolean expressions with some simple nesting
lazy val BExp: Parser[String, BExp] = 
   (AExp ~ "==" ~ AExp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
   (AExp ~ "!=" ~ AExp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
   (AExp ~ "<" ~ AExp) ==> { case x ~ _ ~ z => Bop("<", x, z): BExp } || 
   (AExp ~ ">" ~ AExp) ==> { case x ~ _ ~ z => Bop(">", x, z): BExp } ||
   ("(" ~ BExp ~ ")" ~ "&&" ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => And(y, v): BExp } ||
   ("(" ~ BExp ~ ")" ~ "||" ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => Or(y, v): BExp } ||
   ("true" ==> (_ => True: BExp )) || 
   ("false" ==> (_ => False: BExp )) ||
   ("(" ~ BExp ~ ")") ==> { case _ ~ x ~ _ => x }

// statement / statements
lazy val Stmt: Parser[String, Stmt] =
  (("skip" ==> (_ => Skip: Stmt)) ||
   (IdParser ~ ":=" ~ AExp) ==> { case x ~ _ ~ z => Assign(x, z): Stmt } ||
   ("write(" ~ IdParser ~ ")") ==> { case _ ~ y ~ _ => Write(y): Stmt } ||
   ("if" ~ BExp ~ "then" ~ Block ~ "else" ~ Block) ==>
    { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w): Stmt } ||
   ("while" ~ BExp ~ "do" ~ Block) ==> { case _ ~ y ~ _ ~ w => While(y, w) }) 
 
lazy val Stmts: Parser[String, Block] =
  (Stmt ~ ";" ~ Stmts) ==> { case x ~ _ ~ z => x :: z : Block } ||
  (Stmt ==> ( s => List(s) : Block))

// blocks (enclosed in curly braces)
lazy val Block: Parser[String, Block] =
  (("{" ~ Stmts ~ "}") ==> { case x ~ y ~ z => y} || 
   (Stmt ==> (s => List(s))))

  val fib = """n := 10;
             minus1 := 0;
             minus2 := 1;
             temp := 0;
             while (n > 0) do {
                 temp := minus2;
                 minus2 := minus1 + minus2;
                 minus1 := temp;
                 n := n - 1
             };
             result := minus2""".replaceAll("\\s+", "")

Stmts.parse_all(fib)

