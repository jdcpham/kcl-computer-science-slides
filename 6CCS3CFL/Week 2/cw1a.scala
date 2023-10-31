/* TODO:    Set of Characters [c1, c2..., cn] DONE
            One or more times r+ DONE
            Optional r? DONE
            Exactly N times r{n} DONE
            No more than M times r{..m} Done
            At least N times r{n..} Done
            Between N and M times r{n..m} Done
            Not Regular Expression ~r Done
*/
abstract class Rexp
case object ZERO extends Rexp          
case object ONE extends Rexp                   
case class CHAR(c: Char) extends Rexp          
case class ALT(r1: Rexp, r2: Rexp) extends Rexp  
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp  
case class STAR(r: Rexp) extends Rexp     

case class RANGE(s: Set[Char]) extends Rexp // DONE
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class UPTO(r: Rexp, m: Int) extends Rexp
case class FROM(r: Rexp, n: Int) extends Rexp
case class BETWEEN(r: Rexp, n: Int, m: Int) extends Rexp
case class NOT(r: Rexp) extends Rexp 



/* TODO:    Set of Characters [c1, c2..., cn] 
            One or more times r+ 
            Optional r? 
            Exactly N times r{n} 
            No more than M times r{..m} 
            At least N times r{n..} 
            Between N and M times r{n..m} 
            Not Regular Expression ~r 
*/
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case RANGE(s) => false
  case PLUS(r) => nullable(r)
  case OPTIONAL(r) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  case UPTO(r, _) => true
  case FROM(r, i) => if (i == 0) true else nullable(r)
  case BETWEEN(r, i, _) => if (i == 0) true else nullable(r)
  case NOT(r) => !nullable(r) 
}

def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
                    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
                    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case RANGE(s) =>  if (s.size == 1 && c == s.head) ONE 
                    else if (s.size == 0) ZERO
                    else RANGE(s.filter(!_.equals(c)))
  case NTIMES(r, i) => if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case FROM(r, i) => if (i == 0) FROM(r, i) else SEQ(der(c, r), FROM(r, i - 1))
  case UPTO(r, i) => if (i == 0) ZERO else SEQ(der(c, r), UPTO(r, i - 1)) //CHECK
  case BETWEEN(r, i, j) => if (i == 0) BETWEEN(r, i, j - 1) else SEQ(der(c, r), BETWEEN(r, i, j - 1)) //CHECK
  case NOT(r) => NOT(der(c, r))
  //case PLUS
  //case OPTIONAL
}







def ders (s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, der(c, r))
}

def matcher(r: Rexp, s: String) : Boolean = nullable(ders(s.toList, r))