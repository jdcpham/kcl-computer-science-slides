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

implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S](q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}



case object NumParser extends Parser[List[Token], Int] {
  def parse(sb: List[Token]) = sb match {
    case T_NUM(n)::rest => Set((n.toInt, rest))
    case _ => Set() 
  }
}

NumParser.parse(List(T_NUM(1)))

case object IdParser extends Parser[List[Token], String] {
   def parse(sb: List[Token]) = sb match {
    case T_ID(s)::rest => Set((s, rest))
    case _ => Set()
   } 
}

implicit def TokenOps(t: Token) = new {
  def ~(r: Token) = new SeqParser[List[Token], List[Token], List[Token]](t, r)
}

implicit def StringOps(s: String) = new {
  def ||(q : => Parser[List[Token], String]) = new AltParser[String, String](s, q)
  def ||(r: String) = new AltParser[String, String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
  def ~[S](q : => Parser[String, S]) = 
    new SeqParser[String, String, S](s, q)
  def ~(r: String) = 
    new SeqParser[String, String, String](s, r)
}


//arithmetic expressions
lazy val AExp: Parser[List[Token], AExp] = 
  (Te ~ T_OP("+") ~ AExp) ==> { case x ~ _ ~ z => Aop("+", x, z): AExp } ||
  (Te ~ T_OP("-") ~ AExp) ==> { case x ~ _ ~ z => Aop("-", x, z): AExp } || Te 
lazy val Te: Parser[List[Token], AExp] = 
  (Fa ~ T_OP("*") ~ Te) ==> { case x ~ _ ~ z => Aop("*", x, z): AExp } || 
  (Fa ~ T_OP("/") ~ Te) ==> { case x ~ _ ~ z => Aop("/", x, z): AExp } || Fa  
lazy val Fa: Parser[List[Token], AExp] = 
   (T_LPAREN ~ AExp ~ T_RPAREN) ==> { case _ ~ y ~ _ => y } || 
   IdParser ==> Var || 
   NumParser ==> Num

lazy val Fa: Parser[List[Token], AExp] =
  (T_LPAREN ~ Fa ~ T_RPAREN) ==> { case _ ~ y ~ _ => y }
