// the type of the Control Stack
trait Instruction   

// the type of the Results Stack
trait Result        

// Initial Symbol of Abstract Syntax
trait Program extends Instruction with Result   

// Operators
abstract class Operator extends Instruction
case object Plus extends Operator
case object Minus extends Operator
case object Times extends Operator
case object Divide extends Operator

// Locations or Variables
case class Location(id: String) extends Result

// Integer Expressions
abstract class Exp extends Program
case class ValueAt(loc: Location) extends Exp
case class Num(n: Int) extends Exp
case class BinOp(e1: Exp, op: Operator, e2: Exp) extends Exp

// Configurations
type Control = List[Instruction]
type Results = List[Result]
type Memory = Map[Location, Int]
type Configuration = (Control, Results, Memory)



