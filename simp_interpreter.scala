// the type of the Control Stack
trait Instruction   

// the type of the Results Stack
trait Result        

// Initial Symbol of Abstract Syntax
trait Program extends Instruction with Result   

// Operators
trait Operator extends Instruction {
    def evaluate(n1: Int, n2: Int) : Int
}
case object Plus extends Operator { 
    override def evaluate(n1: Int, n2: Int) : Int = n1 + n2
}
case object Minus extends Operator { 
    override def evaluate(n1: Int, n2: Int) : Int = n1 - n2
}
case object Times extends Operator { 
    override def evaluate(n1: Int, n2: Int) : Int = n1 * n2
}
// Division by 0 defined as 0
case object Divide extends Operator { 
    override def evaluate(n1: Int, n2: Int) : Int = 
        if (n2 != 0) n1 / n2 else 0
}

// Locations or Variables
case class Location(id: String) extends Result

// Integer Expressions
trait Exp extends Program
case class ValueAt(loc: Location) extends Exp
case class Num(n: Int) extends Exp
case class BinOp(e1: Exp, op: Operator, e2: Exp) extends Exp

// Configurations
type Control = List[Instruction]
type Results = List[Result]
type Memory = Map[Location, Int]
type Configuration = (Control, Results, Memory)

// Interpreter - recursively computes transformations from configurations
// Currently has "unnecessary steps" from literally following transition rules
def run(conf: Configuration) : Configuration = conf match {
    case (Num(x)::c, r, m) => 
        run((c, Num(x)::r, m))
    case (BinOp(e1, op, e2)::c, r, m) => 
        run((e1::e2::op::c, r, m))
    case ((op:Operator)::c, Num(n2)::Num(n1)::r, m) =>
        val n = op.evaluate(n1, n2)
        run((c, Num(n)::r, m))    
    case (ValueAt(loc)::c, r, m) =>    
        val n = m(loc)
        run((c, Num(n)::r, m))
    case (Nil, r, m) =>
        conf    
}

val e = BinOp(Num(10), Times, Num(10))
val init : Configuration = (e::Nil, Nil, Map())
run(init)
