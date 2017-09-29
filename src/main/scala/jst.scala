package jst

import scala.util.parsing.combinator._
import scala.annotation.tailrec
import org.apache.commons.lang3.StringEscapeUtils._

abstract class JST {
  def print(out: java.io.Writer)

  def indent(out: java.io.Writer, level: Int) {
    out.write("\n")
    for (i <- 0 until level) {
      out.write(" ")
    }
  }

  def isSimple(s: Int) = true

  def pprint(out: java.io.Writer, level: Int, step: Int = 1, simply: Int = 36) {
    print(out)
  }
}

object JST {
  val Null  = JSTNull()
  val True  = JSTTrue()
  val False = JSTFalse()

  def concat(a: JST, b: JST): JST = {
    (a, b) match {
      case (JSTString(a), JSTString(b)) => JSTString(a + b)
      case (JSTString(a), JSTNum(b)) => JSTString(a + b.toString)
      case (JSTNum(a), JSTString(b)) => JSTString(a.toString + b)
      case (JSTNum(a), JSTNum(b)) => JSTString(a.toString + b.toString)
      case (JSTArr(a), JSTArr(b)) => JSTArr(a ++ b)
      case (JSTObj(a), JSTObj(b)) => JSTObj(a ++ b)
      case _ => throw new Exception("Different types concatinated")
    }
  }
}

case class JSTString(value: String) extends JST {
  override def isSimple(s: Int) = value.length < s

  def print(out: java.io.Writer) {
    out.write("\"")
    out.write(escapeJava(value))
    out.write("\"")
  }
}

case class JSTNum(value: BigDecimal) extends JST {
  def this(s: String) = this(BigDecimal(s))

  def print(out: java.io.Writer) {
    out.write(value.toString)
  }
}

object JSTNum {
  def apply(s: String) = new JSTNum(s)
}

case class JSTArr(value: Seq[JST]) extends JST {
  def print(out: java.io.Writer) {
    out.write("[")
    if (value.nonEmpty) {
      value.head.print(out)
      for (v <- value.tail) {
        out.write(",")
        v.print(out)
      }
    }
    out.write("]")
  }

  override def isSimple(s: Int) = value.length <= 16 && value.forall(_.isSimple(s / value.length))

  override def pprint(out: java.io.Writer, level: Int, step: Int = 1, simply: Int = 36) {
    if (value.nonEmpty) {
      if (isSimple(simply)) {
        out.write("[ ")
        value.head.pprint(out, 0, step, simply)
        for (v <- value.tail) {
          out.write(", ")
          v.pprint(out, 0, step, simply)
        }
        out.write(" ]")
      } else {
        val l = level + step
        out.write("[")
        indent(out, l)
        value.head.pprint(out, l, step, simply)
        for (v <- value.tail) {
          out.write(",")
          indent(out, l)
          v.pprint(out, l, step, simply)
        }
        indent(out, level)
        out.write("]")
      }
    } else {
      out.write("[]")
    }
  }
}

case class JSTObj(value: Seq[(String, JST)]) extends JST {
  def print(out: java.io.Writer) {
    out.write("{")
    if (value.nonEmpty) {
      val vs = value.sortBy(_._1)
      val (k, v) = vs.head
      out.write("\"")
      out.write(escapeJson(k))
      out.write("\":")
      v.print(out)
      for ((k, v) <- vs.tail) {
        out.write(",\"")
        out.write(escapeJson(k))
        out.write("\":")
        v.print(out)
      }
    }
    out.write("}")
  }

  override def isSimple(s: Int) = value.length <= 8 && value.forall({ p => p._2.isSimple(s / value.length - p._1.length) })

  override def pprint(out: java.io.Writer, level: Int, step: Int = 1, simply: Int = 36) {
    if (value.nonEmpty) {
      val vs = value.sortBy(_._1)
      val (k, v) = vs.head
      if (isSimple(simply)) {
        out.write("{ \"")
        out.write(escapeJson(k))
        out.write("\": ")
        v.pprint(out, 0, step, simply)
        for ((k, v) <- vs.tail) {
          out.write(", \"")
          out.write(escapeJson(k))
          out.write("\": ")
          v.pprint(out, 0, step, simply)
        }
        out.write(" }")
      } else {
        val l = level + step
        out.write("{")
        indent(out, l)
        out.write("\"")
        out.write(escapeJson(k))
        out.write("\": ")
        v.pprint(out, l, step, simply)
        for ((k, v) <- vs.tail) {
          out.write(",")
          indent(out, l)
          out.write("\"")
          out.write(escapeJson(k))
          out.write("\": ")
          v.pprint(out, l, step, simply)
        }
        indent(out, level)
        out.write("}")
      }
    } else {
      out.write("{}")
    }
  }
}

abstract class JSTBool extends JST {
  val value: Boolean
}

object JSTBool {

  import JST._

  def apply(v: Boolean): JSTBool = if (v) {
    True
  } else {
    False
  }
}

case class JSTTrue() extends JSTBool {
  val value = true

  def print(out: java.io.Writer) {
    out.write("true")
  }
}

case class JSTFalse() extends JSTBool {
  val value = false

  def print(out: java.io.Writer) {
    out.write("false")
  }
}

case class JSTNull() extends JST {
  def print(out: java.io.Writer) {
    out.write("null")
  }
}

class JSTParser(library: Function[String, String]) extends JavaTokenParsers /*with RegexParsers*/ {

  import JSTParser._
  import JST._

  def getValue(n: String, args: Function[String, JST]) = {
    val res = parse(value(args), library(n))
    if( res.successful ) {
      res.get
    } else {
      throw new Exception(res.toString)
    }
  }

  val simpleStr             = stringLiteral ^^ { s =>
    unescapeJson(s.substring(1, s.length - 1))
  }
  val key                   = ident | simpleStr
  val comment: Parser[Unit] = ("/\\*.*?\\*/".r | "//[^\n].*".r) ^^ { _ => () }
  protected override val whiteSpace = "(?:\\s|(?:/\\*.*?\\*/)|(?://[^\n].*))+".r
  // (super.whiteSpace ^^ { _ => () }) | comment
  val jSep   = ",".? ^^ { _ => () }
  val str    = simpleStr ^^ JSTString
  val num    = floatingPointNumber ^^ { s: String => JSTNum(s) }
  val jNull  = "null" ^^^ Null
  val jTrue  = "true" ^^^ True
  val jFalse = "false" ^^^ False

  def value(args: Function[String, JST]): Parser[JST] = obj(args) | arr(args) | str | num | jNull | jTrue | jFalse | include(args) | arg(args) | union(args)

  def arr(args: Function[String, JST]): Parser[JSTArr] = "[" ~> (repsep(value(args), jSep) <~ jSep.? <~ "]") ^^ JSTArr

  def obj(args: Function[String, JST]): Parser[JSTObj] = "{" ~> (repsep((key <~ ":") ~ value(args) ^^ { case k ~ v => k -> v }, jSep) <~ jSep.? <~ "}") ^^ JSTObj

  def include(args: Function[String, JST]): Parser[JST] = ("@\\(\\s*include\\s+".r ~> ((ident | simpleStr) ~ (ident ~ ("=" ~> value(args))).*) <~ ")") ^^ { case n ~ args => getValue(n, args.map({ case n ~ v => (n -> v) }).toMap) }

  def arg(args: Function[String, JST]): Parser[JST] = "$" ~> ident ^^ args

  def union(args: Function[String, JST]): Parser[JST] = "@\\(concat\\s+".r ~> (repsep(value(args), jSep) <~ jSep.? <~ ")") ^^ (_.reduce(concat))
}

object JSTParser {
  def forget(j: JST) = j
}

class FSStorage(path: Seq[java.nio.file.Path], log: Boolean = false) extends Function[String, String] {

  import FSStorage._

  def apply(n: String) = {
    val f = scala.io.Source.fromFile(find(n, path, log))
    val s = f.mkString
    f.close()
    s
  }
}

object FSStorage {
  @tailrec def find(name: String, path: Seq[java.nio.file.Path], log: Boolean): java.io.File = {
    //    System.err.println(s"parse $name")
    if (path.isEmpty) {
      System.err.println(s"$name not found")
      throw new Exception(s"$name not found")
    } else {
      val f = path.head.resolve(name).toFile
      if (f.exists) {
        if (log) {
          System.err.println(s"found ${f}")
        }
        f
      } else {
        find(name, path.tail, log)
      }
    }
  }

  def apply(p: Seq[String], log: Boolean = false) = new FSStorage(p map { s => (new java.io.File(s)).toPath }, log)
}
