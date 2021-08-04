import cats.implicits.{
  catsSyntaxApplicativeId,
  catsSyntaxFunction1FlatMap,
  catsSyntaxSemigroup
}
import cats.{Applicative, FlatMap, Id, Semigroup}

// Type of the graph
trait GraphType[A] {
  def graph: A
  def diGraph: A
}
// Definition of subgraph
trait SubgraphDefine[A] {
  def _false: A
  def _true: A
}

// Rank of the graph
trait GraphRank[A] {
  def same: A
  def source: A
  def sink: A
  def min: A
  def max: A
}
// Rank dir of the graph
trait DirRank[A] {
  def tb: A
  def bt: A
  def lr: A
  def rl: A
}
// Shape of the node
trait NodeShape[A] {
  def circle: A
  def doublecircle: A
  def box: A
  def plaintext: A
  def msquare: A
  def record: A
}
// Style of the line
trait LineStyle[A] {
  def solid: A
  def bold: A
  def filled: A
  def invis: A
  def dotted: A
  def dashed: A
}
// Type of the arrow
trait ArrowType[A] {
  def normal: A
  def inv: A
  def none: A
}
// Value of the constraint edge
trait ConstraintEdge[A] {
  def _false: A
  def _true: A
}

// Main interface for building graph
trait GraphBuilder[F[_], A] {
  //Auxiliary func
  def push(addedStr: A, suffix: String): A => F[A]

  //Header func
  def open(name: Option[String] = None): A => F[A]
  def comment(comment: String): A => F[A]
  def label(label: String): A => F[A]
  def rank(rank: GraphRank[String] => String): A => F[A]
  def rankdir(rankdir: DirRank[String] => String): A => F[A]
  def color(color: String): A => F[A]

  def edge(
      src: String,
      dst: String,
      style: Option[LineStyle[A] => A] = None,
      arrowHead: Option[ArrowType[A] => A] = None,
      constraint: Option[ConstraintEdge[A] => A] = None
  ): A => F[A]
  def node(
      name: String,
      shape: Option[NodeShape[A] => A],
      style: Option[LineStyle[A] => A] = None,
      color: Option[String] = None,
      label: Option[String] = None
  ): A => F[A]
  def close: A => F[A]
}

object StrGraphBuilder {

  // Interpreter of basic operations
  private class StrGraphType extends GraphType[String] {
    override def graph: String = {
      edgeDraw = " -- "
      "graph"
    }
    override def diGraph: String = {
      edgeDraw = " -> "
      "diGraph"
    }
  }

  private class StrGraphRank extends GraphRank[String] {
    override def same = "same"
    override def source = "source"
    override def sink = "sink"
    override def min = "min"
    override def max = "max"
  }

  private class StrDirRank extends DirRank[String] {
    override def tb = "tb"
    override def bt = "bt"
    override def lr = "lr"
    override def rl = "rl"
  }

  private class StrNodeShape extends NodeShape[String] {
    override def circle = "circle"
    override def doublecircle = "doublecircle"
    override def box = "box"
    override def plaintext = "plaintext"
    override def msquare = "msquare"
    override def record = "record"
  }

  private class StrLineStyle extends LineStyle[String] {
    override def solid = "solid"
    override def bold = "bold"
    override def filled = "filled"
    override def invis = "invis"
    override def dotted = "dotted"
    override def dashed = "dashed"
  }

  private class StrArrowType extends ArrowType[String] {
    override def normal = "normal"
    override def inv = "inv"
    override def none = "none"
  }

  private class StrConstraintEdge extends ConstraintEdge[String] {
    override def _false = "false"
    override def _true = "true"
  }

  private val strGraphType = new StrGraphType
  private val strGraphRank = new StrGraphRank
  private val strDirRank = new StrDirRank
  private val strNodeShape = new StrNodeShape
  private val strLineStyle = new StrLineStyle
  private val strArrowType = new StrArrowType
  private val strConstraintEdge = new StrConstraintEdge

  def apply[F[_]: Applicative](
      grtype: GraphType[String] => String
  ): StrGraphBuilder[F] = {
    gtype = grtype(strGraphType)
    new StrGraphBuilder[F]
  }
  private def quote(str: String): String =
    str match {
      case _ if str.startsWith("\"") => str
      case _                         => s""""$str""""
    }

  def attrMkStr(attr: Map[String, String]): Option[String] =
    if (attr.isEmpty) None
    else Some("[" + attr.map(t => t._1 + "=" + t._2).mkString(" ") + "]")

  private val tab = "  "
  private var gtype = ""
  private var edgeDraw = ""
  private val t = tab
}

class StrGraphBuilder[F[_]: Applicative] extends GraphBuilder[F, String] {
  import StrGraphBuilder._
  override def push(
      addedStr: String,
      suffix: String = "\n"
  ): String => F[String] = { input: String =>
    (input + addedStr + suffix).pure[F]
  }
  override def open(name: Option[String]): String => F[String] = {
    val str_name: String = name.map(n => quote(n)).getOrElse("")
    push(s"$gtype $str_name {")
  }
  override def comment(comment: String): String => F[String] = {
    push(s"// $comment")
  }
  override def label(label: String): String => F[String] = {
    push(s"${t}label = ${quote(label)}")
  }
  override def rank(
      rank: GraphRank[String] => String
  ): String => F[String] = {
    push(s"${t}rank = ${rank(strGraphRank)}")
  }
  override def rankdir(
      rankdir: DirRank[String] => String
  ): String => F[String] = {
    push(s"${t}rankdir = ${rankdir(strDirRank)}")
  }
  override def color(color: String): String => F[String] = {
    push(s"${t}color = $color")
  }
  override def edge(
      src: String,
      dst: String,
      style: Option[LineStyle[String] => String] = None,
      arrowHead: Option[ArrowType[String] => String] = None,
      constraint: Option[ConstraintEdge[String] => String] = None
  ): String => F[String] = {
    val attrStyle: Map[String, String] =
      style.map(s => Map("style" -> s(strLineStyle))).getOrElse(Map.empty)
    val attrArrowHead: Map[String, String] = arrowHead
      .map(s => Map("arrowhead" -> s(strArrowType)))
      .getOrElse(Map.empty)
    val attrConstraint: Map[String, String] = constraint
      .map(s => Map("constraint" -> s(strConstraintEdge)))
      .getOrElse(Map.empty)
    val attrs: Map[String, String] =
      attrStyle |+| attrConstraint |+| attrArrowHead
    push(
      "%s%s%s%s%s".format(
        t,
        quote(src),
        edgeDraw,
        quote(dst),
        attrMkStr(attrs).map(a => " " + a).getOrElse("")
      )
    )
  }
  override def node(
      name: String,
      shape: Option[NodeShape[String] => String] = None,
      style: Option[LineStyle[String] => String] = None,
      color: Option[String] = None,
      label: Option[String] = None
  ): String => F[String] = {
    val attrShape: Map[String, String] =
      shape.map(s => Map("shape" -> s(strNodeShape))).getOrElse(Map.empty)
    val attrStyle: Map[String, String] =
      style.map(s => Map("style" -> s(strLineStyle))).getOrElse(Map.empty)
    val attrColor: Map[String, String] =
      color.map(s => Map("color" -> s)).getOrElse(Map.empty)
    val attrLabel: Map[String, String] =
      label.map(s => Map("label" -> s)).getOrElse(Map.empty)

    val attrs: Map[String, String] =
      attrShape |+| attrColor |+| attrLabel |+| attrStyle
    push(
      "%s%s%s".format(
        t,
        quote(name),
        attrMkStr(attrs).map(a => " " + a).getOrElse("")
      )
    )
  }
  override def close: String => F[String] = {
    push(s"${t.substring(tab.length)}}", suffix = "")
  }
}

object Main extends App {

  def myGraph[F[_]: FlatMap, A: Semigroup](g: GraphBuilder[F, A]): A => F[A] = {
    g.open(Some("name_graph")) >=>
      g.comment("I am comment") >=>
      g.label("label") >=>
      g.rank(o => o.same) >=>
      g.rankdir(o => o.bt) >=>
      g.color("black") >=>
      g.edge(
        "start_edge",
        "finish_edge",
        Some(o => o.filled),
        Some(o => o.normal),
        Some(o => o._false)
      ) >=>
      g.node(
        "node_name",
        Some(o => o.circle),
        Some(o => o.filled),
        Some("red"),
        Some("label_caption")
      ) >=>
      g.close
  }
  val strGraphBuilder =
    StrGraphBuilder[Id](o => o.graph)
  println(myGraph(strGraphBuilder).apply(""))
}
/*
graph "name_graph" {
// I am comment
  label = "label"
  rank = same
  rankdir = bt
  color = black
  "start_edge" -- "finish_edge" [constraint=false style=filled arrowhead=normal]
  "node_name" [color=red shape=circle label=label_caption style=filled]
}
 */
