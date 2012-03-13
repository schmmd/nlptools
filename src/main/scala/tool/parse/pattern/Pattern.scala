package edu.washington.cs.knowitall
package tool
package parse
package pattern

import common._

import graph._
import collection._

import scala.util.matching.Regex

/**
  * Represents a pattern with which graphs can be searched.  
  * A pattern will start and end with a node matcher, and every
  * matcher (necessarily) alternates between a NodeMatcher and
  * and EdgeMatcher.
  */
class Pattern[T](
  /** a list of matchers, alternating between `NodeMatcher`s and `EdgeMatcher`s. */
  val matchers: List[Matcher[T]]
) extends Function[Graph[T], List[Match[T]]] {
  
  require(matchers != null)

  // ensure that the matchers alternate
  matchers.view.zipWithIndex.foreach { case(m, i) => 
    (m, (i%2)) match {
      case (m: NodeMatcher[_], 0) =>
      case (m: EdgeMatcher[_], 1) =>
      case _ => throw new IllegalArgumentException("matchers must start with a node matcher and alternate")
    }
  }

  def this(edgeMatchers: List[EdgeMatcher[T]], nodeMatchers: List[NodeMatcher[T]]) = {
    this(enrich.Iterables.interleave(nodeMatchers, edgeMatchers).toList)
  }

  // extend Object
  override def toString = {
    matchers.view.map(_.toString).mkString(" ") }
  def canEqual(that: Any) = that.isInstanceOf[Pattern[_]]
  override def equals(that: Any) = that match {
    case that: Pattern[_] => (that canEqual this) && this.matchers == that.matchers
    case _ => false
  }
  override def hashCode = this.matchers.hashCode
  
  /** Find all matches of this pattern in the graph. */
  def apply(graph: Graph[T]): List[Match[T]] = {
    graph.vertices.toList.flatMap(apply(graph, _).toList)
  }
  
  def baseEdgeMatchers = {
    this.edgeMatchers.map(_.baseEdgeMatcher)
  }
  
  def baseNodeMatchers = {
    this.nodeMatchers.flatMap(_.baseNodeMatchers)
  }

  /** Find all matches of this pattern in the graph starting at `vertex`. */
  def apply(graph: Graph[T], vertex: T): List[Match[T]] = {
    def rec(matchers: List[Matcher[T]], 
      vertex: T, 
      edges: List[DirectedEdge[T]],
      nodeGroups: List[(String, Match.NodeGroup[T])],
      edgeGroups: List[(String, Match.EdgeGroup[T])]): List[Match[T]] = matchers match {

      case (m: CaptureNodeMatcher[_]) :: xs =>
        m.matchText(vertex).map(text => rec(xs, vertex, edges, (m.alias, Match.NodeGroup(vertex, text)) :: nodeGroups, edgeGroups)).getOrElse(List())
      case (m: NodeMatcher[_]) :: xs if m.matches(vertex) => 
        if (m.matches(vertex)) rec(xs, vertex, edges, nodeGroups, edgeGroups)
        else List()
      case (m: EdgeMatcher[_]) :: xs => 
        // only consider edges that have not been used
        val uniqueEdges = graph.dedges(vertex)--edges.flatMap(e => List(e, e.flip))
        // search for an edge that matches
        uniqueEdges.flatMap { edge => 
          m.matchText(edge).map(text => (edge, text))
        }.flatMap { case (dedge, matchText) =>
          val groups = m match {
            case m: CaptureEdgeMatcher[_] => (m.alias, Match.EdgeGroup(dedge, matchText)) :: edgeGroups
            case _ => edgeGroups
          }
          // we found one, so recurse
          rec(xs, dedge.end, dedge :: edges, nodeGroups, groups)
        }(scala.collection.breakOut)
      case _ => List(new Match(this, new Bipath(edges.reverse), nodeGroups.toMap, edgeGroups.toMap))
    }

    rec(this.matchers, vertex, List(), List(), List())
  }
  
  /** A list of just the edge matchers, in order. */
  def edgeMatchers = matchers.collect { case m: EdgeMatcher[_] => m }
  /** A list of just the node matchers, in order. */
  def nodeMatchers = matchers.collect { case m: NodeMatcher[_] => m }

  def reflection = new Pattern(this.matchers.reverse.map {
    case m: EdgeMatcher[_] => m.flip
    case m: NodeMatcher[_] => m
  })
}

/**
  * Abstract superclass for all matchers. */
sealed abstract class Matcher[T]

/**
  * Trait to match dependency graph edges. 
  */
sealed abstract class EdgeMatcher[T] extends Matcher[T] {
  def apply(edge: DirectedEdge[T]) = this.matchText(edge)

  def matches(edge: DirectedEdge[T]) = this.matchText(edge).isDefined
  def matchText(edge: DirectedEdge[T]): Option[String]

  def canMatch(edge: Graph.Edge[T]): Boolean = this.matches(new UpEdge(edge)) || this.matches(new DownEdge(edge))
  def flip: EdgeMatcher[T]
  
  def baseEdgeMatcher: BaseEdgeMatcher[T]
}

abstract class BaseEdgeMatcher[T] extends EdgeMatcher[T] {
  override def baseEdgeMatcher = this
}

abstract class WrappedEdgeMatcher[T](val matcher: EdgeMatcher[T]) extends EdgeMatcher[T] {
  def canEqual(that: Any) = that.isInstanceOf[WrappedEdgeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: WrappedEdgeMatcher[_] => (that canEqual this) && this.matcher == that.matcher
    case _ => false
  }
  
  override def baseEdgeMatcher = matcher.baseEdgeMatcher
}

class DirectedEdgeMatcher[T](val direction: Direction, matcher: EdgeMatcher[T]) extends WrappedEdgeMatcher[T](matcher) {
  def matchText(edge: DirectedEdge[T]) = 
    if (edge.dir == direction) matcher.matchText(edge)
    else None

  def flip: EdgeMatcher[T] = new DirectedEdgeMatcher(direction.flip, matcher)
 
  /** symbolic representation used in serialization. */
  def symbol = direction match { 
    case Direction.Up => "<" 
    case Direction.Down => ">" 
  }

  // extend Object
  override def toString = symbol + matcher.toString + symbol
  override def canEqual(that: Any) = that.isInstanceOf[DirectedEdgeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: DirectedEdgeMatcher[_] => (that canEqual this) && this.direction == that.direction && super.equals(that)
    case _ => false
  }
  override def hashCode = direction.hashCode + 39*matcher.hashCode
}

class TrivialEdgeMatcher[T] extends BaseEdgeMatcher[T] {
  def matchText(edge: DirectedEdge[T]) = Some(edge.edge.label)
  def flip = this
}

class CaptureEdgeMatcher[T](val alias: String, matcher: EdgeMatcher[T]) extends WrappedEdgeMatcher[T](matcher) {
  override def matchText(edge: DirectedEdge[T]) = matcher.matchText(edge)
  override def flip = new CaptureEdgeMatcher(alias, matcher.flip)
  
  // extend Object
  override def toString = matcher match {
    case _: TrivialEdgeMatcher[_] => "{"+alias+"}"
    case d: DirectedEdgeMatcher[_] => d.symbol+"{"+alias+":"+d.matcher.toString+"}"+d.symbol
    case m: EdgeMatcher[_] => "{"+alias+":"+m.toString+"}"
  }
  override def canEqual(that: Any) = that.isInstanceOf[CaptureEdgeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: CaptureEdgeMatcher[_] => (that canEqual this) && this.alias == that.alias && super.equals(that)
    case _ => false
  }
  override def hashCode = alias.hashCode + 39*matcher.hashCode
}

/**
  * Trait to match dependency graph nodes. 
  */
sealed abstract class NodeMatcher[T] extends Matcher[T] {
  def apply(node: T) = this.matchText(node)

  def matches(node: T) = this.matchText(node).isDefined
  def matchText(node: T): Option[String]
  
  def baseNodeMatchers: Seq[BaseNodeMatcher[T]]
}

abstract class BaseNodeMatcher[T] extends NodeMatcher[T] {
  override def baseNodeMatchers = Seq(this)
}

abstract class WrappedNodeMatcher[T](val matcher: NodeMatcher[T])
extends NodeMatcher[T] {
  def canEqual(that: Any) = that.isInstanceOf[WrappedNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: WrappedNodeMatcher[_] => (that canEqual this) && this.matcher == that.matcher
    case _ => false
  }
  
  override def baseNodeMatchers = this.matcher.baseNodeMatchers
}

class ConjunctiveNodeMatcher[T](val matchers: Set[NodeMatcher[T]]) 
extends NodeMatcher[T] {
  require(matchers.size > 1)
  require(!matchers.exists(_.isInstanceOf[ConjunctiveNodeMatcher[_]]))
  
  def this(m: NodeMatcher[T]) = this(Set(m))
  def this(m: NodeMatcher[T]*) = this(Set() ++ m)
  
  override def matchText(node: T) = matchers.flatMap(_.matchText(node)).headOption
  override def matches(node: T) = matchers.forall(_.matches(node))
  
  override def baseNodeMatchers = this.matchers.toSeq.flatMap(_.baseNodeMatchers)
  
  override def toString = matchers.mkString(":")
  def canEqual(that: Any) = that.isInstanceOf[ConjunctiveNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: ConjunctiveNodeMatcher[_] => (that canEqual this) && this.matchers == that.matchers
    case _ => false
  }  
}

/**
  * Always match any node. */
class TrivialNodeMatcher[T] extends BaseNodeMatcher[T] {
  override def matchText(node: T) = Some(".*")

  // extend Object
  override def toString = ".*"
  def canEqual(that: Any) = that.isInstanceOf[TrivialNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: TrivialNodeMatcher[_] => that canEqual this
    case _ => false
  }
  override def hashCode = toString.hashCode
}

/**
  * Trait that captures the contents of a node if it's matched.
  * @param  alias  the name of the captured node
  * @param  matcher  the matcher to apply
  */
class CaptureNodeMatcher[T](val alias: String, matcher: NodeMatcher[T]) 
extends WrappedNodeMatcher[T](matcher) {
  /**
    * Convenience constructor that uses the TrivialNodeMatcher.
    */
  def this(alias: String) = this(alias, new TrivialNodeMatcher[T]())

  override def matchText(node: T) = matcher.matchText(node)
  
  // extend Object
  override def toString = "{" +
    (if (matcher.isInstanceOf[TrivialNodeMatcher[_]]) {
      alias
    }
    else {
      alias + ":" + matcher.toString
    }) + "}"
  override def canEqual(that: Any) = that.isInstanceOf[CaptureNodeMatcher[_]]
  override def equals(that: Any) = that match {
    case that: CaptureNodeMatcher[_] => (that canEqual this) && this.alias == that.alias && super.equals(that)
    case _ => false
  }
  override def hashCode = alias.hashCode + 39*matcher.hashCode
}
