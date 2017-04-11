import scala.io._
import cs162.assign5.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Interpreter {
  type Environment = scala.collection.immutable.HashMap[Var, Value]

  object StuckException extends Exception

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      // parsing error: program is not well-formed
      case Left(e) => println(e)
      // successfully parsed: program is well-formed
      case Right(program) => try {

        // we only run well-typed programs, so ready to interpret
        var curr_state = State(program.e, new Environment(), Seq())
        while ( !curr_state.fin ) curr_state = curr_state.next

        curr_state.t match {
          case v:Value ⇒
            println(s"Result: ${PrettyValue.prettyVal(v)}")
          case e:Exp ⇒
            // Should never happen
            println(s"Error: unevaluated expression resulted: ${Pretty.prettyExp(e)}")
        }
      } catch {
        case StuckException ⇒ println("Program is stuck")
      }
    }
  }
}

import Interpreter._

case class State(t: Term, env: Environment, ks: Seq[Kont]) {

  def fin: Boolean = t match{
    case v: Value => v match{
      case LetRecV(_,_,_) => false
      case _ => ks.isEmpty
    }
    case _ => false 
  }

  def next: State = t match {
    case e:Exp => {
      e match {
        // variables
        case x:Var => State(env.getOrElse(x, throw StuckException), env, ks)

        // numeric literals
        case Num(n) => State(NumV(n), env, ks)

        // boolean literals
        case Bool(b) => State(BoolV(b), env, ks)

        // `nil` - the literal for unit
        case _:NilExp => State(NilV, env, ks)

        // builtin arithmetic operators
        case Plus | Minus | Times | Divide | LT | EQ | And | Or | Not => throw StuckException // You shouldn't change this.

        // function creation
        case Fun(params, body) => State(ClosureV(params.map(_._1).toSeq, body, env), env, ks)

        // function call
        case Call(fun, args) => fun match {
          case Not => State(args.head, env, NotK +: ks)
          case op:Builtin => State(args.head, env, BinopLeftK(op, args.last) +: ks)
          case _ => State(fun, env, AppK(args, Seq[Value]()) +: ks)
        }

        // conditionals 
        case If(e1, e2, e3) => State(e1, env, IfK(e2, e3) +: ks)

        // let binding
        case Let(x, e1, e2) => State(e1, env, LetK(x, e2) +: ks)

        // recursive binding
        case Rec(x, _, e1, e2) => State(e2, env + ((x,LetRecV(x, e1, env))), RestoreK(env) +: ks)

        // record literals
        case Record(fields) => State(fields.values.head, env,  RecordK(fields.keys.toSeq, fields.values.toSeq.drop(1), Seq[Value]()) +: ks)

        // record access
        case Access(e, field) => State(e, env, AccessK(field) +: ks)

        // constructor use
        case Construct(constructor, e) => State(e, env, ConsK(constructor) +: ks)

        // pattern matching (case ... of ...)
        case Match(e, cases) => State(e, env, CaseK(cases) +: ks)
      }
    }
    case v:Value => {
      v match {
        case LetRecV(x, e1, env1) => State(e1, env1 + ((x,LetRecV(x, e1, env1))), RestoreK(env) +: ks)
        case _ => ks.head match {

          // restore continuation
          case RestoreK(env1) => State(v, env1, ks.drop(1))

          // binary operation continuations
          case BinopLeftK(op, e) => State(e, env, BinopRightK(op, v) +: ks.drop(1))

          case BinopRightK(op, v1) => State(valueOf(op, v1, v), env, ks.drop(1))

          // not operation continuation
          case NotK => v match{
            case BoolV(true) => State(BoolV(false), env, ks.drop(1))
            case BoolV(false) => State(BoolV(true), env, ks.drop(1))
            case _ => throw StuckException
          }

          // function application continuation
          case AppK(argsE, vals) =>
            if(argsE.isEmpty) (v +: vals).reverse match{
              case ClosureV(xi, e, p) :: vargs => 
                if(vargs.size == xi.size) State(e, p ++ (xi zip vargs).toMap, RestoreK(env) +: ks.drop(1))
                else throw StuckException
              case _ => throw StuckException
              }
            else State(argsE.head, env, AppK(argsE.drop(1), v +: vals) +: ks.drop(1))

          // if continuation
          case IfK(e2, e3) => v match{
            case BoolV(true) => State(e2, env, ks.drop(1))
            case BoolV(false) => State(e3, env, ks.drop(1))
            case _ => throw StuckException
          }

          // let continuation
          case LetK(x, e2) => State(e2, env + ((x,v)), RestoreK(env) +: ks.drop(1))

          // record continuation
          case RecordK(fields, es, vals) =>
            if(es.isEmpty) State(RecordV((fields zip (v +: vals).reverse).toMap), env, ks.drop(1))
            else State(es.head, env, RecordK(fields, es.drop(1), v +: vals) +: ks.drop(1))

          // access continuation
          case AccessK(field) => t match{
            case RecordV(rec) => State(rec.getOrElse(field, throw StuckException), env, ks.drop(1))
            case _ => throw StuckException
          }

          // constructing user-defined data type continuation
          case ConsK(constructor) => State(ConstructorV(constructor, v), env, ks.drop(1))

          // case continuation
          case CaseK(cases) => t match{
            case ConstructorV(cons, v) => 
              val target = cases.filter(_._1 == cons).head 
              State(target._3, env + ((target._2, v)), RestoreK(env) +: ks.drop(1))
            case _ => throw StuckException
          }
        }
      }
    }
  }

  def valueOf(op: Builtin, v1: Value, v2: Value): Value = (v1, v2) match{
    case (num1:NumV, num2:NumV) => op match{
      case Plus   => NumV(num1.n + num2.n)
      case Minus  => NumV(num1.n - num2.n)
      case Times  => NumV(num1.n * num2.n)
      case Divide => NumV(num1.n / num2.n)
      case LT     => BoolV(num1.n < num2.n)
      case EQ     => BoolV(num1.n == num2.n)
      case _      => throw StuckException
    }
    case (b1:BoolV, b2:BoolV) => op match{
      case And    => BoolV(b1.b && b2.b)
      case Or     => BoolV(b1.b || b2.b)
      case _      => throw StuckException
    }
    case(_,_) => throw StuckException
  }
}

sealed abstract class Value extends Term
case class NumV(n: BigInt) extends Value
case class BoolV(b: Boolean) extends Value
case object NilV extends Value
case class ClosureV(params: Seq[Var], body: Exp, env: Environment) extends Value
case class RecordV(fields: Map[Label, Value]) extends Value
case class ConstructorV(constructor: Label, v: Value) extends Value
case class LetRecV(x: Var, e: Exp, env: Environment) extends Value

sealed abstract class Kont
case object NotK extends Kont
case class BinopLeftK(op: Builtin, e: Exp) extends Kont
case class BinopRightK(op: Builtin, v: Value) extends Kont
case class AppK(argsE: Seq[Exp], vals: Seq[Value]) extends Kont
case class IfK(e2: Exp, e3: Exp) extends Kont
case class LetK(x: Var, e2: Exp) extends Kont
case class RecordK(fields: Seq[Label], es: Seq[Exp], vals: Seq[Value]) extends Kont
case class AccessK(field: Label) extends Kont
case class ConsK(constructor: Label) extends Kont
case class CaseK(cases: Seq[(Label, Var, Exp)]) extends Kont
case class RestoreK(env: Environment) extends Kont

object PrettyValue {
  def prettyVal(v: Value): String = {
    def prettyEnv(env: Environment) = env.map({ case (x, v) ⇒ s"${Pretty.prettyExp(x)} ↦ ${prettyVal(v)}" }).mkString(", ")

    v match {
      case NumV(n) ⇒ n.toString
      case BoolV(b) ⇒ b.toString
      case NilV ⇒ "()"
      case ClosureV(params, body, env) ⇒
        val pparams = params.map(Pretty.prettyExp).mkString(", ")
        s"Closure: ((${pparams}) ⇒ ${Pretty.prettyExp(body)}), ρ = { ${prettyEnv(env)} }"
      case RecordV(fields) ⇒
        val pfields = fields.map { case (f, v) ⇒ s"$f = ${prettyVal(v)}" }
        s"[ ${pfields.mkString(", ")} ]"
      case ConstructorV(constructor, v) ⇒
        s"$constructor ${prettyVal(v)}"
      case LetRecV(x, e, env) ⇒
        s"LetRec: ${Pretty.prettyExp(x)} = ${Pretty.prettyExp(e)}, ρ = { ${prettyEnv(env)} }"
    }
  }
}
