
abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 
case class UPNTIMES(r: Rexp, n: Int) extends Rexp 

// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case UPNTIMES(r: Rexp, n: Int) => true
}

// the derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r1, i) => 
    if (i == 0) ZERO else
    if (nullable(r1)) SEQ(der(c, r1), UPNTIMES(r1, i - 1))
    else SEQ(der(c, r1), NTIMES(r1, i - 1))
  case UPNTIMES(r1, i) => 
    if (i == 0) ZERO
    else SEQ(der(c, r1), UPNTIMES(r1, i - 1)) 
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}


// the derivative w.r.t. a string (iterates der)
def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}


// the main matcher function
def matches(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))

// one or zero
def OPT(r: Rexp) = ALT(r, ONE)

// evil regular expressions
def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))
val EVIL3 = SEQ(STAR(ALT(CHAR('a'), SEQ(CHAR('a'),CHAR('a')))), CHAR('b'))

def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  "%.5f".format((end - start) / (i * 1.0e9))
}


// test: (a?{n}) (a{n})
for (i <- 0 to 8000 by 1000) {
  println(s"$i: ${time_needed(2, matches(EVIL1(i), "a" * i))}")
}

// test: (a*)* b
for (i <- 0 to 6000000 by 500000) {
  println(s"$i: ${time_needed(2, matches(EVIL2, "a" * i))}")
}


val r0 = simp(der('a', EVIL3))
val r1 = simp(der('a', r0))
val r2 = simp(der('a', r1))
val r3 = simp(der('a', r2))
val r4 = simp(der('a', r3))
val r5 = simp(der('a', r4))
val r6 = simp(der('a', r5))

//test: (a|aa)* b
/*
for (i <- 0 to 100 by 10) {
  println(s"$i: ${time_needed(2, matches(EVIL3, "a" * i ++ "c"))}")
}
 */

