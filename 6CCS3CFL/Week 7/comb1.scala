import scala.language.implicitConversions
import scala.language.reflectiveCalls

/* Note, in the lectures I did not show the implicit type 
 * constraint IsSeq, which means that the input type 'I' needs 
 * to be a sequence. */

type IsSeq[A] = A => Seq[_]

abstract class Parser[I : IsSeq, T] {
  def parse(ts: I): Set[(T, I)]

  def parse_all(ts: I) : Set[T] =
    for ((head, tail) <- parse(ts); 
        if tail.isEmpty) yield head
}

class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(sb: I) = 
    for ((head1, tail1) <- p.parse(sb); 
         (head2, tail2) <- q.parse(tail1)) yield ((head1, head2), tail2)
}

class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(sb: I) = p.parse(sb) ++ q.parse(sb)   
}

class FunParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(sb: I) = 
    for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
}

// atomic parsers for characters, numbers and strings
case class CharParser(c: Char) extends Parser[String, Char] {
  def parse(sb: String) = 
    if (sb != "" && sb.head == c) Set((c, sb.tail)) else Set()
}

import scala.util.matching.Regex
case class RegexParser(reg: Regex) extends Parser[String, String] {
  def parse(sb: String) = reg.findPrefixMatchOf(sb) match {
    case None => Set()
    case Some(m) => Set((m.matched, m.after.toString))  
  }
}

val NumParser = RegexParser("[0-9]+".r)
def StringParser(s: String) = RegexParser(Regex.quote(s).r)

// NumParserInt2 transforms a "string integer" into an actual Int;
// needs new, because FunParser is not a case class
val NumParserInt2 = new FunParser(NumParser, (s: String) => s.toInt)


// convenience
implicit def string2parser(s: String) = StringParser(s)
implicit def char2parser(c: Char) = CharParser(c)

implicit def ParserOps[I, T](p: Parser[I, T])(implicit ev: I => Seq[_]) = new {
  def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
}

implicit def StringOps(s: String) = new {
  def || (q : => Parser[String, String]) = new AltParser[String, String](s, q)
  def || (r: String) = new AltParser[String, String](s, r)
  def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
  def ~[S] (q : => Parser[String, S]) = 
    new SeqParser[String, String, S](s, q)
  def ~ (r: String) = 
    new SeqParser[String, String, String](s, r)
}

// NumParserInt can now be written as _ ===> _
// the first part is the parser, and the second the 
// semantic action
val NumParserInt = NumParser ==> (s => s.toInt)


// palindromes
lazy val Pal : Parser[String, String] = 
  (("a" ~ Pal ~ "a") ==> { case ((x, y), z) => x + y + z } ||
   ("b" ~ Pal ~ "b") ==> { case ((x, y), z) => x + y + z } || "a" || "b" || "")

Pal.parse_all("abaaaba")
Pal.parse_all("abacba")
Pal.parse("abaaaba")

println("Palindrome: " + Pal.parse_all("abaaaba"))

// parser for well-nested parentheses (transforms '(' -> '{' , ')' -> '}' )
lazy val P : Parser[String, String] = 
  "(" ~ P ~ ")" ~ P ==> { case (((_, x), _), y) => "{" + x + "}" + y } || ""

P.parse_all("(((()()))())")
P.parse_all("(((()()))()))")
P.parse_all(")(")
P.parse_all("()")

// just counts parentheses
lazy val P2 : Parser[String, Int] = 
  ("(" ~ P2 ~ ")" ~ P2 ==> { case (((_, x), _), y) => x + y + 2 } || 
   "" ==> { _ => 0 })

P2.parse_all("(((()()))())")
P2.parse_all("(((()()))()))")

// counts opening and closing parentheses
lazy val P3 : Parser[String, Int] = 
  ("(" ~ P3 ==> { case (_, x) => x + 1 } ||
   ")" ~ P3 ==> { case (_, x) => x - 1 } || 
   "" ==> { _ => 0 })

P3.parse_all("(((()()))())")
P3.parse_all("(((()()))()))")
P3.parse_all(")(")

// Arithmetic Expressions (Terms and Factors)
// (because it is mutually recursive, you need :paste 
//  for munching this definition in the REPL)

lazy val E: Parser[String, Int] = 
  (T ~ "+" ~ E) ==> { case ((x, y), z) => x + z } ||
  (T ~ "-" ~ E) ==> { case ((x, y), z) => x - z } || T 
lazy val T: Parser[String, Int] = 
  (F ~ "*" ~ T) ==> { case ((x, y), z) => x * z } || F
lazy val F: Parser[String, Int] = 
  ("(" ~ E ~ ")") ==> { case ((x, y), z) => y } || NumParserInt

/* same parser but producing a string
lazy val E: Parser[String, String] = 
  (T ~ "+" ~ E) ==> { case ((x, y), z) => "(" + x + ")+(" + z + ")"} || T 
lazy val T: Parser[String, String] = 
  (F ~ "*" ~ T) ==> { case ((x, y), z) => "(" + x + ")*("+ z + ")"} || F
lazy val F: Parser[String, String] = 
  ("(" ~ E ~ ")") ==> { case ((x, y), z) => y } || NumParser
*/

println(E.parse_all("1+3+4"))
println(E.parse("1+3+4"))
println(E.parse_all("4*2+3"))
println(E.parse_all("4*(2+3)"))
println(E.parse_all("(4)*((2+3))"))
println(E.parse_all("4/2+3"))
println(E.parse("1 + 2 * 3"))
println(E.parse_all("(1+2)+3"))
println(E.parse_all("1+2+3"))  



// no left-recursion allowed, otherwise will loop
lazy val EL: Parser[String, Int] = 
  (EL ~ "+" ~ EL ==> { case ((x, y), z) => x + z} || 
   EL ~ "*" ~ EL ==> { case ((x, y), z) => x * z} ||
   "(" ~ EL ~ ")" ==> { case ((x, y), z) => y} ||
   NumParserInt)

//println(EL.parse_all("1+2+3"))


// non-ambiguous vs ambiguous grammars

// ambiguous
lazy val S : Parser[String, String] =
  ("1" ~ S ~ S) ==> { case ((x, y), z) => x + y + z } || ""

S.parse("1" * 10)

// non-ambiguous
lazy val U : Parser[String, String] =
  ("1" ~ U) ==> { case (x, y) => x + y  } || ""

U.parse("1" * 25)

U.parse("11")
U.parse("11111")
U.parse("11011")

U.parse_all("1" * 100)
U.parse_all("1" * 100 + "0")

lazy val UCount : Parser[String, Int] =
  ("1" ~ UCount) ==> { case (x, y) => y + 1 } || "" ==> { x => 0 }

UCount.parse("11111")
UCount.parse_all("11111")



// Single Character parser
lazy val One : Parser[String, String] = "1"
lazy val Two : Parser[String, String] = "2"

One.parse("1")
One.parse("111")

(One ~ One).parse("111")
(One ~ One ~ One).parse("111")
(One ~ One ~ One ~ One).parse("1111")

(One || Two).parse("111")


// a problem with the arithmetic expression parser -> gets 
// slow with deep nestedness
E.parse("1")
E.parse("(1)")
E.parse("((1))")
E.parse("(((1)))")
E.parse("((((1))))")
E.parse("((((((1))))))")
E.parse("(((((((1)))))))")
