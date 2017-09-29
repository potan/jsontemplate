
import scala.language.implicitConversions

/**
 * https://gist.github.com/gwenzek/78355526e476e08bb34d
 * @author gwenzek
 *
 */

class ArgsOps(ops: Map[String, OptionalParam], val args: Array[String]){
    def apply(op: String) = ops(op)
}

class OptionalParam(val name: String) {
    def asInt() = this match { case IntParam(_, i) => i }
    def asString() = this match { case StringParam(_, s) => s }
    def asBoolean() = this match { case FlagParam(_, b) => b}

    def length() = this match {
        case IntParam(_, _) | StringParam(_, _) => 2
        case FlagParam(_, _) => 1
    }

    def toPairs() = name.split("\\|").map( _ -> this )
}

sealed case class IntParam(override val name: String, val value: Int) extends OptionalParam(name)
sealed case class StringParam(override val name: String, val value: String) extends OptionalParam(name)
sealed case class FlagParam(override val name: String, val value: Boolean = false) extends OptionalParam(name)

object ArgsOps {

    case class OptionWithoutExpectedParam(name: String) extends Exception
    case class UnknownParam(name: String) extends Exception

    class ArgsOpsParser(default: Map[String, OptionalParam]){

        def parse(args: Array[String]) = {

            def parseOne(i: Int, l: List[OptionalParam]): (Int, List[OptionalParam]) = {
                if(i == args.length || args(i)(0) != '-') (i, l)
                else{
                    val partialName = args(i)
                    val op = default.getOrElse(partialName, throw UnknownParam(partialName))
                    if(i + op.length > args.length) throw OptionWithoutExpectedParam(op.name)
                    val received : OptionalParam = op match {
                        case _ : IntParam => try { op.name -> args(i+1).toInt } catch
                            {case e: java.lang.NumberFormatException => throw OptionWithoutExpectedParam(op.name)}
                        case _ : StringParam => 
                            if(args(i+1)(0) == '-') throw OptionWithoutExpectedParam(op.name)
                            else op.name -> args(i+1)
                        case _ : FlagParam => FlagParam(op.name, true)
                    }
                    parseOne(i+op.length, received :: l)
                }
            }
            val (i, l) = parseOne(0, Nil)
            new ArgsOps(default ++ l.flatMap(_.toPairs).toMap, args.slice(i, args.length))
        }

        def <<|(args: Array[String]) = parse(args)
    }

    object ArgsOpsParser {
        def apply(ops: OptionalParam*) = new ArgsOpsParser(ops.flatMap(_.toPairs).toMap)
    }

    implicit def asInt(op: OptionalParam) = op match { case IntParam(_, i) => i }
    implicit def asString(op: OptionalParam) = op match { case StringParam(_, s) => s }
    implicit def asBoolean(op: OptionalParam) = op match { case FlagParam(_, b) => b}

    implicit def fromPairSS(kv: (String, String)): OptionalParam= StringParam(kv._1, kv._2)
    implicit def fromPairSI(kv: (String, Int)): OptionalParam = IntParam(kv._1, kv._2)
    implicit def fromString(k: String): OptionalParam = FlagParam(k)
    
}

import jst._

object main {
  def main(args: Array[String]) = {
    import ArgsOps._
    val ap = ArgsOpsParser(
      "--path|-p" -> ".",
      "--template|-t" -> "main.jst",
      "--output|-o" -> "",
      "--pretty",
      "--log")
    val argsOps = ap <<| args
    val path = argsOps("--path").split(",")
    val tmpl = argsOps("--template").asString

    val p = new JSTParser(FSStorage(path, argsOps("--log").asBoolean))
    val r = p.getValue(tmpl, Map())
    val printer = if( argsOps("--pretty").asBoolean ) {
      w: java.io.Writer => r.pprint(w,0)
    } else {
      w: java.io.Writer => r.print(w)
    }
    argsOps("--output").asString  match {
      case "" =>
        val w = new java.io.StringWriter
        printer(w)
        println(w.toString)
      case f =>
        val w = new java.io.FileWriter(f)
        printer(w)
        w.close()
    }
  }
}
