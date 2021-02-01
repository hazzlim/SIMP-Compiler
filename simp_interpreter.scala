// the type of the Control Stack
trait Instruction   

// the type of the Results Stack
trait Result        

// Initial Symbol of Abstract Syntax
trait Program extends Instruction with Result   

// Locations or Variables
case class Location(id: String) extends Result

// Integer Operators
trait Operator extends Instruction 
case object Plus extends Operator 
case object Minus extends Operator 
case object Times extends Operator 
case object Divide extends Operator 

// Evaluates Integer operations - division by 0 defined as 0
def evaluate_integer(n1: Int, op: Operator, n2: Int) : Num = op match {
    case Plus => Num(n1 + n2)
    case Minus => Num(n1 - n2)
    case Times => Num(n1 * n2)
    case Divide => if (n2 != 0) Num(n1 / n2) else Num(0)
}

// Integer Expressions
trait Exp extends Program
case class ValueAt(loc: Location) extends Exp
case class Num(n: Int) extends Exp
case class BinOp(e1: Exp, op: Operator, e2: Exp) extends Exp

// Boolean Operators
trait BooleanOperator extends Instruction 
case object GreaterThan extends BooleanOperator
case object LessThan extends BooleanOperator
case object EqualTo extends BooleanOperator

// Boolean Expressions
trait Bool extends Program
case object True extends Bool 
case object False extends Bool 
case class BoolOp(e1: Exp, op: BooleanOperator, e2: Exp) extends Bool
case class Not(e: Exp) extends Bool
case class And(e1: Exp, e2: Exp) extends Bool

// Evaluates boolean operations
def evaluate_boolean(n1: Int, bop: BooleanOperator, n2: Int) : Bool = bop match {
    case GreaterThan => if (n1 > n2) True else False
    case LessThan => if (n1 < n2) True else False
    case EqualTo => if (n1 == n2) True else False
}

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
        val n = evaluate_integer(n1, op, n2)
        run((c, n::r, m))    
    case (ValueAt(loc)::c, r, m) =>    
        val n = m(loc)
        run((c, Num(n)::r, m))
    case (Nil, r, m) =>
        conf    
}

val e = BinOp(Num(10), Times, Num(10))
val e = BinOp(ValueAt(Location("x")), Plus, Num(1))

val m = Map(Location("x") -> 100)
val c : List[Instruction] = e::Nil
val r : List[Result] = Nil

val init : Configuration = (c, r, m)
run(init)
