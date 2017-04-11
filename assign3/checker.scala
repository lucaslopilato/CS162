import scala.io._
import cs162.assign3.syntax._
import Aliases._
import scala.io.Source.fromFile

//—————————————————————————————————————————————————————————————————————————
// Main entry point

object Checker {
  type TypeEnv = scala.collection.immutable.HashMap[Var, Type]
  object Illtyped extends Exception

  var typeDefs = Set[TypeDef]()

  def main( args:Array[String] ) {
    val filename = args(0)
    val input = fromFile(filename).mkString
    Parsers.program.run(input, filename) match {
      case Left(e) => println(e)
      case Right(program) =>
        val prettied = Pretty.prettySyntax(program)
        typeDefs = program.typedefs

        try {
          getType( program.e, new TypeEnv())
          println(Pretty.prettySyntax(program))
          println("This program is well-typed:\n")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets all the constructors associated with a given type name.
  // For example, consider the following typedefs:
  //
  // type Either = Left num | Right bool
  // type Maybe = Some num | None
  //
  // With respect to the above typedefs, `constructors` will return
  // the following underneath the given arguments:
  //
  // constructors(Label("Either")) = Map(Label("Left") -> NumT, Label("Right") -> BoolT)
  // constructors(Label("Maybe")) = Map(Label("Some") -> NumT, Label("None") -> UnitT)
  // constructors(Label("Fake")) throws Illtyped
  //
  def constructors(name: Label): Map[Label, Type] =
    typeDefs.find(_.name == name).map(_.constructors).getOrElse(throw Illtyped)

  // Gets the type of the constructor.
  // For example, considering the typedefs given in the `constructors` comment above,
  // `typename` will return the following with the given arguments:
  //
  // typename(Label("Left")) = Label("Either")
  // typename(Label("Right")) = Label("Either")
  // typename(Label("Some")) = Label("Maybe")
  // typename(Label("None")) = Label("Maybe")
  //
  def typename(constructor: Label): Label =
    typeDefs.find(_.constructors.contains(constructor)).getOrElse(throw Illtyped).name

  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      // variables
      case x:Var => env.getOrElse(x, throw Illtyped)

      // numeric literals
      case _:Num =>  NumT

      // boolean literals
      case _:Bool => BoolT

      // `nil` - the literal for unit
      case _: NilExp => UnitT

      // builtin arithmetic operators
      case Plus | Minus | Times | Divide => FunT(Seq(NumT,NumT),NumT)

      // builtin relational operators
      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)

      // builtin logical operators
      case And | Or => FunT(Seq(BoolT,BoolT),BoolT)

      // builtin logical operators
      case Not => FunT(Seq(BoolT), BoolT)

      // function creation
      case Fun(params, body) => 
        FunT(params.foldLeft(Seq[Type]())((seq,pair) => seq :+ pair._2), getType(body, env ++ params.toMap) )

      // function call
      case Call(fun, args) => getType(fun, env) match{
        case FunT(params, ret) =>
          if(params.corresponds(args.map(getType(_,env)))(_ == _)) ret
          else throw Illtyped //See if the types of the arguments in order equal the arguments of the function
        case _ => throw Illtyped
      }
      // conditionals 
      case If(e1, e2, e3) => (getType(e1,env),getType(e2,env),getType(e3,env)) match {
        case(BoolT, b, c) => if(b == c) b else throw Illtyped
        case(_,_,_) => throw Illtyped
      }

      // let binding
      case Let(x, e1, e2) => getType(e2, env+((x,getType(e1,env))))

      // recursive binding
      case Rec(x, t1, e1, e2) => 
        if(getType(e1,env + ((x,t1))) == t1) 
          getType(e2, env+((x,t1))) 
        else throw Illtyped

      // record literals
      case Record(fields) => RcdT(fields.transform( (key,value) => getType(value,env)))

      // record access
      case Access(e, field) => getType(e,env) match{
        case rcd:RcdT => rcd.fields.getOrElse(field, throw Illtyped)
        case _ => throw Illtyped
      }

      // constructor use
      case Construct(constructor, e) ⇒ 
        if(constructors(typename(constructor)).getOrElse(constructor, throw Illtyped) == getType(e,env))
          TypT(typename(constructor))
        else throw Illtyped

      case Match(e, cases) => getType(e,env) match{
        case TypT(name) => 
          //Create list of all types from cases
          val expTypes = cases.map(c => getType(c._3, env + ((c._2, constructors(name).getOrElse(c._1, throw Illtyped)))))
          
          //check if all cases defined 
          if(constructors(name).keys.toList.sorted == cases.map(_._1).toList.sorted &&
              expTypes.forall(_ == expTypes.head)) //Check if all return types are the same
          expTypes.head 
          else throw Illtyped
          
        case _ => throw Illtyped
      }
    }
}
