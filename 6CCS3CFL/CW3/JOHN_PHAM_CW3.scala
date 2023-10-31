/* Imports */
import scala.language.implicitConversions
import scala.language.reflectiveCalls

/* Define Tokens */
abstract class Token extends Serializable 
case object T_SEMI extends Token
case object T_LPAREN extends Token
case object T_RPAREN extends Token
case object T_CLPAREN extends Token
case object T_CRPAREN extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token

/* Abstract Syntax Tree */
abstract class Stmt
abstract class AExp
abstract class BExp 

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt
case class Write_String(s: String) extends Stmt // New: Differentiates between writing from variable and from String.
case class Read(s: String) extends Stmt         // New: Gets user input.

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


/* Parser Combinators */
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



/* Atomic Parsers */
case class TokenParser(t: Token) extends Parser[List[Token], Token] {
  def parse(sb: List[Token]) = {
    if (sb.length >= 1 && sb.head == t) Set((sb.head, sb.tail));
    else Set();
  }
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

case object IdParser extends Parser[List[Token], String] {
   def parse(sb: List[Token]) = sb match {
    case T_ID(s)::rest => Set((s, rest))
    case _ => Set()
   } 
}

case object StringParser extends Parser[List[Token], String] {
   def parse(sb: List[Token]) = sb match {
    case T_STR(s)::rest => Set((s, rest))
    case _ => Set()
   } 
}

implicit def token2parser(t : Token) = TokenParser(t)


/* Token Operators for Convenience */
implicit def TokenOps(s: Token) = new {

  def ||(q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](s, q)
  def ||(r: Token) = new AltParser[List[Token], Token](s, r)
  def ==>[S] (f: => Token => S) = new FunParser[List[Token], Token, S](s, f)
  def ~[S](q : => Parser[List[Token], S]) = new SeqParser[List[Token], Token, S](s, q)
  def ~(r: Token) = new SeqParser[List[Token], Token, Token](s, r)
}


/* Parser: Arithmetic Expressions */
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

/* Parser: Boolean Expressions */
lazy val BExp: Parser[List[Token], BExp] = 
   (AExp ~ T_OP("==") ~ AExp) ==> { case x ~ _ ~ z => Bop("==", x, z): BExp } || 
   (AExp ~ T_OP("!=") ~ AExp) ==> { case x ~ _ ~ z => Bop("!=", x, z): BExp } || 
   (AExp ~ T_OP("<") ~ AExp) ==> { case x ~ _ ~ z => Bop("<", x, z): BExp } || 
   (AExp ~ T_OP(">") ~ AExp) ==> { case x ~ _ ~ z => Bop(">", x, z): BExp } ||
   (T_LPAREN ~ BExp ~ T_RPAREN ~ T_OP("&&") ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => And(y, v): BExp } ||
   (T_LPAREN ~ BExp ~ T_RPAREN ~ T_OP("||") ~ BExp) ==> { case _ ~ y ~ _ ~ _ ~ v => Or(y, v): BExp } ||
   (T_OP("true") ==> (_ => True: BExp )) || 
   (T_OP("false") ==> (_ => False: BExp )) ||
   (T_LPAREN ~ BExp ~ T_RPAREN) ==> { case _ ~ x ~ _ => x }

/* Parser: Statement */
lazy val Stmt: Parser[List[Token], Stmt] =
  ((T_KWD("skip") ==> (_ => Skip: Stmt)) ||
   (IdParser ~ T_OP(":=") ~ AExp) ==> { case x ~ _ ~ z => Assign(x, z): Stmt } ||
   (T_KWD("if") ~ BExp ~ T_KWD("then") ~ Block ~ T_KWD("else") ~ Block) ==> { case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w): Stmt } ||
   (T_KWD("while") ~ BExp ~ T_KWD("do") ~ Block) ==> { case _ ~ y ~ _ ~ w => While(y, w) } ||
   (T_KWD("read") ~ IdParser) ==> { case _ ~ x => Read(x): Stmt } ||
   (T_KWD("write") ~ IdParser) ==> { case _ ~ y => Write(y): Stmt } ||
   (T_KWD("write") ~ StringParser) ==> { case _ ~ y => Write_String(y): Stmt}
  ) 
 
/* Parser: Statements */
lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ T_SEMI ~ Stmts) ==> { case x ~ _ ~ z => x :: z : Block } ||
  (Stmt ==> ( s => List(s) : Block))

/* Parser: Blocks */
lazy val Block: Parser[List[Token], Block] =
  ((T_CLPAREN ~ Stmts ~ T_CRPAREN) ==> { case _ ~ y ~ _ => y } || 
   (Stmt ==> (s => List(s))))




/* Interpreter */
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
  case While(b, bl) => 
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case Write(x) => { println(env(x)) ; env } 
  case Write_String(x) => { println(x); env } 
  case Read(x) => env + (x -> Console.in.readLine().toInt)
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())





/* Question 2 (Working) */
val q2 =  List(   
                  T_KWD("write"), T_STR("Enter value for a"), T_SEMI,
                  T_KWD("read"), T_ID("a"), T_SEMI,
                  T_KWD("write"), T_STR("Enter value for b"), T_SEMI,
                  T_KWD("read"), T_ID("b"), T_SEMI,
                  T_KWD("if"), T_LPAREN, T_ID("a"), T_OP("<"), T_ID("b"), T_RPAREN,
                  T_KWD("then"), T_CLPAREN,
                  T_KWD("skip"), T_CRPAREN,
                  T_KWD("else"), T_CLPAREN,
                  T_ID("a"), T_OP(":="), T_ID("a"), T_OP("*"), T_ID("b"), T_OP("+"), T_NUM(1),
                  T_CRPAREN)

Stmts.parse_all(q2)
eval(Stmts.parse_all(q2).head)





/* Loops (Working) */
val loops =
  List(
        T_ID("start"), T_OP(":="), T_NUM(1000), T_SEMI,
        T_ID("x"), T_OP(":="), T_ID("start"), T_SEMI,
        T_ID("y"), T_OP(":="), T_ID("start"), T_SEMI, 
        T_ID("z"), T_OP(":="), T_ID("start"), T_SEMI,
        T_KWD("while"), T_NUM(0), T_OP("<"), T_ID("x"), T_KWD("do"), T_CLPAREN,
        T_KWD("while"), T_NUM(0), T_OP("<"), T_ID("y"), T_KWD("do"), T_CLPAREN,
        T_KWD("while"), T_NUM(0), T_OP("<"), T_ID("z"), T_KWD("do"), T_CLPAREN,
        T_ID("z"), T_OP(":="), T_ID("z"), T_OP("-"), T_NUM(1), T_CRPAREN, T_SEMI,
        T_ID("z"), T_OP(":="), T_ID("start"), T_SEMI,
        T_ID("y"), T_OP(":="), T_ID("y"), T_OP("-"), T_NUM(1), T_CRPAREN, T_SEMI,
        T_ID("y"), T_OP(":="), T_ID("start"), T_SEMI,
        T_ID("x"), T_OP(":="), T_ID("x"), T_OP("-"), T_NUM(1), T_CRPAREN)

Stmts.parse_all(loops)

/* Used for timing */
val t1 = System.nanoTime
eval(Stmts.parse_all(loops).head)
val duration = (System.nanoTime - t1) / 1e9d





/* Fibonacci (Working) */
val fib =
  List(
        T_KWD("write"), T_STR("Fib"), T_SEMI,
        T_KWD("read"), T_ID("n"), T_SEMI,
        T_ID("minus1"), T_OP(":="), T_NUM(0), T_SEMI,
        T_ID("minus2"), T_OP(":="), T_NUM(1), T_SEMI,
        T_KWD("while"), T_LPAREN, T_ID("n"), T_OP(">"), T_NUM(0), T_RPAREN, T_KWD("do"), T_CLPAREN,
        T_ID("temp"), T_OP(":="), T_ID("minus2"), T_SEMI,
        T_ID("minus2"), T_OP(":="), T_ID("minus1"), T_OP("+"), T_ID("minus2"), T_SEMI,
        T_ID("minus1"), T_OP(":="), T_ID("temp"), T_SEMI,
        T_ID("n"), T_OP(":="), T_ID("n"), T_OP("-"), T_NUM(1), T_CRPAREN, T_SEMI,
        T_KWD("write"), T_STR("Result"), T_SEMI, 
        T_KWD("write"), T_ID("minus2"), T_SEMI,
        T_ID("result"), T_OP(":="), T_ID("minus2"))

Stmts.parse_all(fib)
eval(Stmts.parse_all(fib).head)
println(eval(Stmts.parse_all(fib).head)("result"))





/* Factors (Working) */
val factors = 
  List(
        T_KWD("write"), T_STR("Input n please"), T_SEMI,
        T_KWD("read"), T_ID("n"), T_SEMI,
        T_KWD("write"), T_STR("The factors of n are"), T_SEMI,
        T_ID("f"), T_OP(":="), T_NUM(2), T_SEMI,
        T_KWD("while"), T_LPAREN, T_ID("n"), T_OP("!="), T_NUM(1), T_RPAREN, T_KWD("do"), T_CLPAREN, 
        T_KWD("while"), T_LPAREN, T_ID("n"), T_OP("/"), T_ID("f"), T_RPAREN, T_OP("*"), T_ID("f"), T_OP("=="), T_ID("n"), T_KWD("do"), T_CLPAREN, 
        T_KWD("write"), T_ID("f"), T_SEMI,
        T_ID("n"), T_OP(":="), T_ID("n"), T_OP("/"), T_ID("f"), T_CRPAREN, T_SEMI,
        T_ID("f"), T_OP(":="), T_ID("f"), T_OP("+"), T_NUM(1), T_CRPAREN)

Stmts.parse_all(factors)
eval(Stmts.parse_all(factors).head)





/* Prime Numbers (Working) */
val primes =
  List(
        T_ID("end"), T_OP(":="), T_NUM(100), T_SEMI,
        T_ID("n"), T_OP(":="), T_NUM(2), T_SEMI,
        T_KWD("while"), T_LPAREN, T_ID("n"), T_OP("<"), T_ID("end"), T_RPAREN, T_KWD("do"), T_CLPAREN,
        T_ID("f"), T_OP(":="), T_NUM(2), T_SEMI, 
        T_ID("tmp"), T_OP(":="), T_NUM(0), T_SEMI,
        T_KWD("while"), T_LPAREN, T_LPAREN, T_ID("f"), T_OP("<"), T_ID("n"), T_OP("/"), T_NUM(2), T_OP("+"), T_NUM(1), T_RPAREN, T_OP("&&"),
        T_LPAREN, T_ID("tmp"), T_OP("=="), T_NUM(0), T_RPAREN, T_RPAREN, T_KWD("do"), T_CLPAREN,
        T_KWD("if"), T_LPAREN, T_LPAREN, T_ID("n"), T_OP("/"), T_ID("f"), T_RPAREN, T_OP("*"), T_ID("f"), T_OP("=="), T_ID("n"), T_RPAREN, T_KWD("then"), T_CLPAREN,
        T_ID("tmp"), T_OP(":="), T_NUM(1), T_CRPAREN, 
        T_KWD("else"), T_CLPAREN,
        T_KWD("skip"), T_CRPAREN, T_SEMI,
        T_ID("f"), T_OP(":="), T_ID("f"), T_OP("+"), T_NUM(1), T_CRPAREN, T_SEMI, 
        T_KWD("if"), T_LPAREN, T_ID("tmp"), T_OP("=="), T_NUM(0), T_RPAREN, T_KWD("then"), T_CLPAREN,
        T_KWD("write"), T_ID("n"), T_CRPAREN,
        T_KWD("else"), T_CLPAREN,
        T_KWD("skip"), T_CRPAREN, T_SEMI,
        T_ID("n"), T_OP(":="), T_ID("n"), T_OP("+"), T_NUM(1), T_CRPAREN)

Stmts.parse_all(primes)
eval(Stmts.parse_all(primes).head) 








