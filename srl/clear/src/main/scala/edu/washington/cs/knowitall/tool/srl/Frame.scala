package edu.washington.cs.knowitall.tool.srl

import edu.washington.cs.knowitall.collection.immutable.Interval
import edu.washington.cs.knowitall.tool.tokenize.Token
import edu.washington.cs.knowitall.tool.parse.graph.DependencyNode

case class Frame(relation: Relation, arguments: Seq[Argument]) {
  override def toString = relation.toString + ":" + arguments.mkString("[", ", ", "]")
}
case class Relation(node: DependencyNode, name: String, sense: String) {
  override def toString = name + "." + sense
}
object Relation {
  def fromString(node: DependencyNode, string: String) = {
    val Array(name, sense) = string.split("\\.")
    Relation(node, name, sense)
  }
}
case class Argument(node: DependencyNode, role: Role) {
  override def toString = role + "=" + node.string
}

abstract class Role(val description: String) {
  def label = this.getClass.getSimpleName.replaceAll("_", "-").takeWhile(_ != '$')
}
object Roles {
  def apply(label: String) = {
    val APattern = """A(\d+)""".r
    val CPattern = """C-A(\d+)""".r
    val RPattern = """R-A(\d+)""".r
    label match {
      case "A0" => A0
      case "A1" => A1
      case "A2" => A2
      case "A3" => A3
      case "A4" => A4
      case "A5" => A5
      case CPattern(n) => C(n.toInt)
      case RPattern(n) => R(n.toInt)
      case "AM-ADV" => AM_ADV
      case "AM-DIR" => AM_DIR
      case "AM-DIS" => AM_DIS
      case "AM-EXT" => AM_EXT
      case "AM-LOC" => AM_LOC
      case "AM-MNR" => AM_MNR
      case "AM-MOD" => AM_MOD
      case "AM-NEG" => AM_NEG
      case "AM-PRD" => AM_PRD
      case "AM-PRP" => AM_PRP
      case "AM-REC" => AM_REC
      case "AM-TMP" => AM_TMP
      case "C" => C_ARG
      case "R" => R_ARG
      case _ => UnknownRole(label)
    }
  }
  case object A0 extends Role("subject")
  case object A1 extends Role("object")
  case object A2 extends Role("indirect object")
  case object A3 extends Role("???")
  case object A4 extends Role("???")
  case object A5 extends Role("???")
  case class C(n: Int) extends Role("continuation")
  case class R(n: Int) extends Role("reference")
  case object AM_ADV extends Role("adverbial modification")
  case object AM_DIR extends Role("direction")
  case object AM_DIS extends Role("discourse marker")
  case object AM_EXT extends Role("extent")
  case object AM_LOC extends Role("location")
  case object AM_MNR extends Role("manner")
  case object AM_MOD extends Role("general modification")
  case object AM_NEG extends Role("negation")
  case object AM_PRD extends Role("secondary predicate")
  case object AM_PRP extends Role("purpose")
  case object AM_REC extends Role("recipricol")
  case object AM_TMP extends Role("temporal")
  case object C_ARG extends Role("continuity of an argument/adjunct of type arg")
  case object R_ARG extends Role("reference to an actual argument/adjunct of type arg")
  case class UnknownRole(override val label: String) extends Role(label)
}