// A Small Compiler for the WHILE Language
// (it does not use a parser nor lexer)


// the abstract syntax trees
abstract class Stmt
abstract class AExp
abstract class BExp 
type Block = List[Stmt]

// statements
case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class For(a: Assign, u: AExp, bl: Block) extends Stmt
case class Write(s: String) extends Stmt
case class Write_String(s: String) extends Stmt
case class Read(s: String) extends Stmt

// arithmetic expressions
case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

// boolean expressions
case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp


// compiler headers needed for the JVM
// (contains an init method, as well as methods for read and write)
val beginning = """
.class public XXX.XXX
.super java/lang/Object

.method public <init>()V
   aload_0
   invokenonvirtual java/lang/Object/<init>()V
   return
.end method

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2 
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0
    invokevirtual java/io/PrintStream/println(I)V 
    return 
.end method

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V
    return
.end method 

.method public static read()I 
    .limit locals 10 
    .limit stack 10

    ldc 0 
    istore 1  ; this will hold our final integer 
Label1: 
    getstatic java/lang/System/in Ljava/io/InputStream; 
    invokevirtual java/io/InputStream/read()I 
    istore 2 
    iload 2 
    ldc 10   ; the newline delimiter 
    isub 
    ifeq Label2 
    iload 2 
    ldc 32   ; the space delimiter 
    isub 
    ifeq Label2

    iload 2 
    ldc 48   ; we have our digit in ASCII, have to subtract it from 48 
    isub 
    ldc 10 
    iload 1 
    imul 
    iadd 
    istore 1 
    goto Label1 
Label2: 
    ;when we come here we have our integer computed in local variable 1 
    iload 1 
    ireturn 
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

; COMPILED CODE STARTS

"""

val ending = """
; COMPILED CODE ENDS
   return

.end method
"""

// Compiler functions


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// convenient string interpolations 
// for instructions and labels
import scala.language.implicitConversions
import scala.language.reflectiveCalls

implicit def sring_inters(sc: StringContext) = new {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
}

// this allows you to write things like
// i"add" and l"Label"


// environments 
type Env = Map[String, Int]


def compile_op(op: String) = op match {
  case "+" => i"iadd"
  case "-" => i"isub"
  case "*" => i"imul"
  case "/" => i"idiv"
}

// arithmetic expression compilation
def compile_aexp(a: AExp, env : Env) : String = a match {
  case Num(i) => i"ldc $i"
  case Var(s) => i"iload ${env(s)} \t\t; $s"
  case Aop(op, a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ compile_op(op)
}

def getAVal(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s).toInt
  case Aop("+", a1, a2) => (getAVal(a1, env) + getAVal(a2, env))
  case Aop("-", a1, a2) => (getAVal(a1, env) - getAVal(a2, env))
  case Aop("/", a1, a2) => (getAVal(a1, env) / getAVal(a2, env))
  case Aop("*", a1, a2) => (getAVal(a1, env) * getAVal(a2, env))
}


// boolean expression compilation
//  - the jump-label is for where to jump if the condition is not true
def compile_bexp(b: BExp, env : Env, jmp: String) : String = b match {
  case True => ""
  case False => i"goto $jmp"
  case Bop("==", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpne $jmp"
  case Bop("!=", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpeq $jmp"
  case Bop("<", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmpge $jmp"
  case Bop(">", a1, a2) => 
    compile_aexp(a1, env) ++ compile_aexp(a2, env) ++ i"if_icmple $jmp"
}

def getAssignVar(a: Assign) : String = a match {
  case Assign(x, a) => x
  case _ => ""
}


// statement compilation
def compile_stmt(s: Stmt, env: Env) : (String, Env) = s match {
  case Skip => ("", env)
  case Assign(x, a) => {
    val index = env.getOrElse(x, env.keys.size)
    (compile_aexp(a, env) ++ i"istore $index \t\t; $x", env + (x -> index))
  } 
  case If(b, bl1, bl2) => {
    val if_else = Fresh("If_else")
    val if_end = Fresh("If_end")
    val (instrs1, env1) = compile_block(bl1, env)
    val (instrs2, env2) = compile_block(bl2, env1)
    (compile_bexp(b, env, if_else) ++
     instrs1 ++
     i"goto $if_end" ++
     l"$if_else" ++
     instrs2 ++
     l"$if_end", env2)
  }
  case While(b, bl) => {
    val loop_begin = Fresh("Loop_begin")
    val loop_end = Fresh("Loop_end")
    val (instrs1, env1) = compile_block(bl, env)
    (l"$loop_begin" ++
     compile_bexp(b, env, loop_end) ++
     instrs1 ++
     i"goto $loop_begin" ++
     l"$loop_end", env1)
  }
  case For(a, u, bl) => {

    // Variable name of for loop counter
    val fromVar : String = getAssignVar(a);

    // Assign AExp to Variable in environment
    val (instrs1, env1) = compile_stmt(a, env);

    // Upto Value
    val upToVal : Int = getAVal(u, env) + 1;

    // Original Block + Add Statement to Increment Variable by 1
    val bl2 : Block = List.concat(bl, List(Assign(fromVar, Aop("+", Var(fromVar), Num(1)))));

    // Generate While Statement
    val whileStmt : Stmt = While(Bop("<", Var(fromVar), Num(upToVal)), bl2); 

    // Get instructions for while
    val (instrs2, env2) = compile_stmt(whileStmt, env1);

    (instrs1 ++ instrs2, env2)
  }

  case Write(x) => 
    (i"iload ${env(x)} \t\t; $x" ++ 
     i"invokestatic XXX/XXX/write(I)V", env)

  case Write_String(x) =>
     (i"""ldc "${x}"""" ++
     i"invokestatic XXX/XXX/writes(Ljava/lang/String;)V", env)
     
  case Read(x) => {
    val index = env.getOrElse(x, env.keys.size) 
    (i"invokestatic XXX/XXX/read()I" ++ 
     i"istore $index \t\t; $x", env + (x -> index))
  }
}


// compilation of a block (i.e. list of instructions)
def compile_block(bl: Block, env: Env) : (String, Env) = bl match {
  case Nil => ("", env)
  case s::bl => {
    val (instrs1, env1) = compile_stmt(s, env)
    val (instrs2, env2) = compile_block(bl, env1)
    (instrs1 ++ instrs2, env2)
  }
}

// main compilation function for blocks
def compile(bl: Block, class_name: String) : String = {
  val instructions = compile_block(bl, Map.empty)._1
  (beginning ++ instructions ++ ending).replaceAllLiterally("XXX", class_name)
}


// compiling and running files
//
// JVM files can be assembled with 
//
//    java -jar jvm/jasmin-2.4/jasmin.jar fib.j
//
// and started with
//
//    java fib/fib



import scala.util._
import scala.sys.process._
import scala.io

def compile_tofile(bl: Block, class_name: String) = {
  val output = compile(bl, class_name)
  val fw = new java.io.FileWriter(class_name + ".j") 
  fw.write(output) 
  fw.close()
}

def compile_all(bl: Block, class_name: String) : Unit = {
  compile_tofile(bl, class_name)
  println("compiled ")
  val test = ("java -jar jasmin-2.4/jasmin.jar " + class_name + ".j").!!
  println("assembled ")
}


def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start)/(i * 1.0e9)
}


def compile_run(bl: Block, class_name: String) : Unit = {
  println("Start compilation")
  compile_all(bl, class_name)
  println("running")
  println("Time: " + time_needed(1, ("java " + class_name + "/" + class_name).!))
}


var forr = 
  List(
    For(
      Assign("i", Num(2)), Num(4),
      List(
        Write_String("Loop 1"),
        Write("i"),
        For(
          Assign("i", Num(2)), Num(10),
            List(
              Write_String("Loop 2"),
              Write("i")
            )
          )
      )
    )
  );

compile_run(forr, "forr")


val loops =
  List( Assign("start",Num(100)),
        Assign("x",Var("start")),
        Assign("y",Var("start")),
        Assign("z",Var("start")),
        While(Bop("<",Num(0),Var("x")),
        List(While(Bop("<",Num(0),Var("y")),
        List(While(Bop("<",Num(0),Var("z")),
        List(Assign("z",Aop("-",Var("z"),Num(1))))),
        Assign("z",Var("start")),
        Assign("y",Aop("-",Var("y"),Num(1))))),
        Assign("y",Var("start")),
        Assign("x",Aop("-",Var("x"),Num(1)))))
      )


val fib = 
  List(
    Read("n"),
    Assign("minus1",Num(0)),
    Assign("minus2",Num(1)),
    While(
      Bop(">",Var("n"),Num(0)),
      List(
        Assign("temp",Var("minus2")),
        Assign("minus2",Aop("+",Var("minus1"),Var("minus2"))),
        Assign("minus2",Var("temp")),
        Assign("n",Aop("-",Var("n"),Num(1)))
      )
    ),
    Write_String("Result"),
    Write("minus2"),
    Assign("result",Var("minus2"))
  )

  compile_all(fib, "fib")

val factor = 
  List(
    Write_String("Input n please"),
    Read("n"),
    Assign("f", Num(2)),
    While(Bop("!=", Var("n"), Num(1)),
    List(
      While(
        Bop("==", Aop("*", Aop("/", Var("n"), Var("f")), Var("f")),Var("n")),
        List(
          Write("f"),
          Assign("n", Aop("/",Var("n"),Var("f"))
    ))), Assign("f", Aop("+", Var("f"), Num(1))))))

compile_all(factor, "factor")


