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
  def derive(char: Char): Regex = run(Seq(re), Seq(PushDerive), char)

  

  //----------------------------------------------------------------------------
  // Private details.
  //----------------------------------------------------------------------------

  // Derives a regular expression from the top of 'operands' w.r.t. 'char'.
  @annotation.tailrec
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

        //Push Regex
        case PushRe(x) => {
          val finalops = operands :+ x
          return run(finalops, newprog, char)
        }

        //Concatenate
        case PushConcatenate => {
          assert(operands.size > 1)
          val op2 = operands.last
          val temp = operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1~op2)
          return run(finalops, finalprog, char)
        }

        //Union
        case PushUnion => {
          assert(operands.size > 1)
          val op2 = operands.last
          val temp= operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1|op2)
          return run(finalops, finalprog, char)
        }

        //Intersect
        case PushIntersect => {
          val op2 = operands.last
          val temp= operands.init
          val op1 = temp.last
          val finalops = temp.init
          val finalprog = newprog :+ PushRe(op1&op2)
          return run(finalops, finalprog, char)
        }

        //Compliment
        case PushComplement => {
          val op1 =operands.last
          val finalops = operands.init
          val finalprog= newprog :+ PushRe(!op1)
          return run(finalops, finalprog, char)
        }

        //Nullable
        case PushNullable => {
          val op1 =operands.last
          val finalops = operands.init
          val finalprog= newprog :+ PushRe(op1.nullable)
          return run(finalops, finalprog, char)
        }

        //Derive w.r.t. char
        case PushDerive => {
          val op1 = operands.last
          val temp = operands.init

          //Perform one step of derivation on op1
          op1 match {

            //Union w.r.t. char
            case Union(re1, re2) => {
              val finalops = temp
              val prog2 = newprog :+ PushUnion
              val prog3 = prog2 :+ PushDerive
              val prog4 = prog3 :+ PushRe(re2)
              val prog5 = prog4 :+ PushDerive
              val finalprog = prog5 :+ PushRe(re1)
              return run(finalops, finalprog, char)
            }

            //Concatenation
            case Concatenate(a, b) => {
              val finalops = temp
              val prog2 = newprog :+ PushUnion
              val prog3 = prog2 :+ PushConcatenate
              val prog4 = prog3 :+ PushDerive
              val prog5 = prog4 :+ PushRe(b)
              val prog6 = prog5 :+ PushNullable
              val prog7 = prog6 :+ PushRe(a)
              val prog8 = prog7 :+ PushConcatenate
              val prog9 = prog8 :+ PushRe(b)
              val prog10 = prog9 :+ PushDerive
              val finalprog = prog10 :+ PushRe(a)
              return run(finalops, finalprog, char)
            }

            //Intersection
            case Intersect(a, b) => {
              val finalops = temp
              val prog2 = newprog :+ PushIntersect
              val prog3 = prog2 :+ PushDerive
              val prog4 = prog3 :+ PushRe(b)
              val prog5 = prog4 :+ PushDerive
              val finalprog = prog5 :+ PushRe(a)
              return run(finalops, finalprog, char)
            }

            //KleeneStar w.r.t. char
            case KleeneStar(r) => {
              val finalops = temp
              val prog2 = newprog :+ PushConcatenate
              val prog3 = prog2 :+ PushRe(op1)
              val prog4 = prog3 :+ PushDerive
              val finalprog = prog4 :+ PushRe(r)
              return run(finalops, finalprog, char)
            }

            //Compliment
            case Complement(r) => {
              val finalops = temp
              val prog2 = newprog :+ PushComplement
              val prog3 = prog2 :+ PushDerive
              val finalprog = prog3 :+ PushRe(r)
              return run(finalops, finalprog, char)
            }

            //Empty lang
            case `∅` => {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }
            //Singleton Character w.r.t. char
            case Chars(c) if(c.contains(char)) => {
              val finalops = temp :+ EmptyString
              return run(finalops, newprog, char)
            }

            //Anyother charset where char is not in the set
            case Chars(c) if(!c.contains(char)) => {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }

            //The Emptystring
            case EmptyString =>  {
              val finalops = temp :+ Chars()
              return run(finalops, newprog, char)
            }




          }

        }
      }
      
    }
  }
}
