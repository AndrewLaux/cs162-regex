// A virtual machine implementation of derivative-based matching.

package edu.ucsb.cs.cs162.regex.derivative

import edu.ucsb.cs.cs162.regex._

object `package` {
  // Programs for the DerivativeMachine.
  type Program = Seq[Instruction]

  // Pretty-print derivative virtual machine programs.
  def programToString(prog: Program): String = {
    val strs = for (inst <- prog) yield inst match {
      case `PushDerive` => "derive"
      case `PushConcatenate` => "concatenate"
      case `PushUnion` => "union"
      case `PushComplement` => "complement"
      case `PushIntersect` => "intersect"
      case `PushNullable` => "nullable"
      case PushRe(re) => "push " + re.toString
    }

    strs.mkString("\n")
  }
}

// Instructions for the virtual machine.
//
// - Derive: pop the top of the operand stack, compute its derivative w.r.t. the
//   machine's given char, then push the result back on the operand stack.
// - PushConcatenate: pop the top two elements of the operand stack and push
//   their concatenation back on.
// - PushUnion: pop the top two elements of the operand stack and push their
//   union back on.
// - PushComplement: pop the top of the operand stack, take its complement, and
//   push the result back on.
// - PushIntersect: pop the top two elements of the operand stack and push
//   their intersection back on.
// - PushNullable: pop the top of the operand stack, compute its nullability,
//   and push the result back on the operand stack.
// - PushRe(re): push re onto the top of the operand stack.
sealed abstract class Instruction
case object PushDerive extends Instruction
case object PushConcatenate extends Instruction
case object PushUnion extends Instruction
case object PushComplement extends Instruction
case object PushIntersect extends Instruction
case object PushNullable extends Instruction
case class PushRe(re: Regex) extends Instruction

class DerivativeMachine(re: Regex) {
  import Regex._

  //----------------------------------------------------------------------------
  // Public API.
  //----------------------------------------------------------------------------

  

  // Returns true iff 'str' is recognized by 're'.
  def eval(str: String): Boolean = {
    val instruct = Seq(PushDerive)
    return str.foldLeft(re)((currentRe, char) => run(Seq(currentRe), instruct, char)).nullable == ε
  }

    

    

  // Returns the derivative of 're' w.r.t. 'char'.
  def derive(char: Char): Regex = ???

  

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  //@annotation.tailrec
  private def run(operands: Seq[Regex], program: Program, char: Char): Regex = {

    //Program completed
    if (program.isEmpty) {
      assert(operands.size == 1)
      return operands.head
    }

    //Perform instruction
    else {
      val instruct = program.last
      val newprog = program.init

      //Interpret instructions
      instruct match{

        //Concatenate
        case PushConcatenate => {
          val op1 = operands.last
          val temp = operands.init
          val op2 = temp.last
          val temp2 = temp.init
          val finalops = temp2 :+ op1~op2
          return run(finalops, newprog, char)
        }
      }
      
    }
  }
}
