import scala.io._
import cs162.assign4.syntax._
import Aliases._
import scala.io.Source.fromFile

//——————————————————————————————————————————————————————————————————————————————
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
          println("This program is well-typed")
        } catch { case Illtyped => println("This program is ill-typed") }
    }
  }

  // Gets a listing of the constructor names associated with a given type definition.
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructors`, along with return values:
  //
  // constructors("Either") = Set("Left", "Right")
  // constructors("Foo") = a thrown Illtyped exception
  //
  def constructors(name: Label): Set[Label] =
    typeDefs.find(_.name == name).map(_.constructors.keySet).getOrElse(throw Illtyped)

  // Takes the following parameters:
  // -The name of a user-defined constructor
  // -The types which we wish to apply to the constructor
  // Returns the type that is held within the constructor.
  //
  // For example, consider the following type definition:
  //
  // type Either['A, 'B] = Left 'A | Right 'B
  //
  // Some example calls to `constructorType`, along with return values:
  //
  // constructorType("Left", Seq(NumT, BoolT)) = NumT
  // constructorType("Right", Seq(NumT, BoolT)) = BoolT
  // constructorType("Left", Seq(NumT)) = a thrown Illtyped exception
  // constructorType("Right", Seq(BoolT)) = a thrown Illtyped exception
  // constructorType("Foo", Seq(UnitT)) = a thrown Illtyped exception
  // constructorType("Left", Seq(UnitT)) = a thrown Illtyped exception
  //
  def constructorType(constructor: Label, types: Seq[Type]): Type =
    (for {
      td <- typeDefs
      rawType <- td.constructors.get(constructor)
      if (types.size == td.tvars.size)
    } yield replace(rawType, td.tvars.zip(types).toMap)).headOption.getOrElse(throw Illtyped)

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

  // Given a type and a mapping of type variables to other types, it
  // will recursively replace the type variables in `t` with the
  // types in `tv2t`, if possible.  If a type variable isn't
  // in `tv2t`, it should simply return the original type.  If a
  // `TFunT` is encountered, then whatever type variables it defines
  // (the first parameter in the `TFunT`) should overwrite whatever is in
  // `tv2t` right before a recursive `replace` call.  In other words,
  // type variables can shadow other type variables.
  //
  def replace( t:Type, tv2t:Map[TVar, Type] ): Type =
    t match {
      //Base Case Returns 
      case NumT | BoolT | UnitT => t

      //Replace all parameters by the type
      case FunT(params, ret) => FunT(params.map(replace(_, tv2t)), replace(ret, tv2t))

      case RcdT(fields) => RcdT(fields.mapValues(replace(_, tv2t)))

      case TypT(name, typs) => TypT(name, typs.map(replace(_, tv2t)))

      case tv:TVar => tv2t.getOrElse(tv, t)

      case TFunT(tvars, funt) => 
      //Remove all values from tv2t that are in tvars
      replace(funt, tv2t -- tvars) match{
        //Make sure return value is a FunT
        case a:FunT => TFunT(tvars, a)
        case _ => throw Illtyped
      }
    }

  // HINT - the bulk of this remains unchanged from the previous assignment.
  // Feel free to copy and paste code from your last submission into here.
  def getType( e:Exp, env:TypeEnv ): Type =
    e match {
      case x:Var => env.getOrElse(x, throw Illtyped)

      case _:Num => NumT

      case _:Bool => BoolT

      case _:Unit => UnitT

      case Plus | Minus | Times | Divide => FunT(Seq(NumT,NumT),NumT)

      case LT | EQ => FunT(Seq(NumT, NumT), BoolT)

      case And | Or => FunT(Seq(BoolT,BoolT),BoolT)

      case Not => FunT(Seq(BoolT), BoolT)

      case Fun(params, body) => 
        FunT(params.foldLeft(Seq[Type]())((seq,pair) => seq :+ pair._2), getType(body, env ++ params.toMap) )


      case Call(fun, args) => getType(fun, env) match{
        case FunT(params, ret) =>
          if(params.corresponds(args.map(getType(_,env)))(_ == _)) ret
          else throw Illtyped //See if the types of the arguments in order equal the arguments of the function
        case _ => throw Illtyped
      }

      case If(e1, e2, e3) => (getType(e1,env),getType(e2,env),getType(e3,env)) match {
        case(BoolT, b, c) => if(b == c) b else throw Illtyped
        case(_,_,_) => throw Illtyped
      }

      case Let(x, e1, e2) => getType(e2, env+((x,getType(e1,env))))

      case Rec(x, t1, e1, e2) => 
        if(getType(e1,env + ((x,t1))) == t1) 
          getType(e2, env+((x,t1))) 
        else throw Illtyped

      case Record(fields) => RcdT(fields.transform( (key,value) => getType(value,env)))

      case Access(e, field) => getType(e,env) match{
        case rcd:RcdT => rcd.fields.getOrElse(field, throw Illtyped)
        case _ => throw Illtyped
      }

      case Construct(constructor, typs, e) => 
        if(constructorType(constructor, typs) == getType(e, env))
          TypT(typename(constructor), typs)
        else throw Illtyped


      case Match(e, cases) => getType(e, env) match{
        case TypT(name, typs) => 
          //Get all return types of every expression
          val expTypes = cases.map(c => getType(c._3, env + ((c._2, constructorType(c._1, typs)))))

          //Make sure all cases for the constructor are accounted for
          if (constructors(name).toList.sorted == cases.map(_._1).toList.sorted &&
              //Make sure return value is of one type
              expTypes.forall(_ == expTypes.head) )
            expTypes.head //Return the return type
          else throw Illtyped
        case _ => throw Illtyped
      }

      case TAbs(tvars, fun) => getType(fun, env) match{
        case funt:FunT => TFunT(tvars, funt)
        case _ => throw Illtyped
      }

      case TApp(e, typs) => getType(e, env) match{
        //Zipping Approach taken from
        //https://stackoverflow.com/questions/28335495/scala-iterate-over-two-arrays
        case TFunT(tvars, funt) => replace(funt, (tvars, typs).zipped.toMap)
        case _ => throw Illtyped
      }
    }
}
