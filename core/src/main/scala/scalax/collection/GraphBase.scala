package scalax.collection

import language.{higherKinds, implicitConversions}
import collection.{Set, SortedSet, SortedMap}
import util.Random

import GraphPredef.{EdgeLikeIn, NodeParam, OuterNode, InnerNodeParam, OuterEdge, InnerEdgeParam}
import GraphEdge.{EdgeLike, EdgeCompanionBase, DiHyperEdgeLike, EdgeCopy}
import generic.AnyOrdering
import mutable.ArraySet
import interfaces.ExtSetMethods

/**
 * Base template trait for graphs.
 * 
 * This trait provides the common structure and base operations for immutable graphs
 * independently of their representation. Base operations also cover one-step traversals.
 * For unlimited traversals see `trait GraphTraversal`.
 *  
 * Users of Graph usually don't interact directly with this trait but with
 * `trait Graph` instead which inherits the functionality provided by this trait.
 * 
 * If `E` inherits `DirectedEdgeLike` the graph is directed, otherwise it is undirected or mixed.
 * 
 * @tparam N    the user type of the nodes (vertices) in this graph.
 * @tparam E    the kind of the edges (links) in this graph.
 * @define INNODES The isolated (and optionally any other) outer nodes that the node set of
 *         this graph is to be populated with.
 * @define INEDGES The outer edges that the edge set of this graph is to be populated with.
 *         Nodes being the end of any of these edges will be added to the node set.
 * 
 * @author Peter Empen
 */
trait GraphBase[N, E[X] <: EdgeLikeIn[X]]
  extends Serializable
{ selfGraph =>
  /**
   * Populates this graph with `nodes` and `edges`.
   * 
   * The implementing class will typically have a constructor with the same parameters
   * which is invoked by `from` of the companion object.
   *
   * @param nodes $INNODES
   * @param edges $INEDGES
   */
  protected def initialize(nodes: Iterable[N],
                           edges: Iterable[E[N] with EdgeCopy[E]]) {
    this.nodes.initialize(nodes, edges)
    this.edges.initialize(edges)
  }
  /**
   * The order - commonly referred to as |G| - of this graph
   * equaling to the number of nodes.
   */
  def order = nodes.size
  /** `true` if this graph has at most 1 node. */
  @inline final def isTrivial = order <= 1
  /** `true` if this graph has at least 2 nodes. */
  @inline final def nonTrivial = ! isTrivial 
  /**
   * The size - commonly referred to as ||G|| - of this graph
   * equaling to the number of edges.
   * 
   * Method `size` is reserved for the number of nodes and edges
   * because `Graph` is also `SetLike` with set elements being nodes or edges.  
   */
  def graphSize = edges.size
  def isDirected: Boolean
  def isHyper: Boolean
  
  sealed trait InnerElem
  type NodeT <: InnerNode with Serializable
  trait Node extends Serializable
  trait InnerNode extends InnerNodeParam[N] with Node with InnerElem {
    /**
     * The outer node as supplied by the user at instantiation time or
     * by adding nodes this graph.
     *
     * @return Reference to the user-supplied outer node.
     */
    def value: N
    /**
     * All edges at this node - commonly denoted as E(v).
     * 
     * @return all edges with at least one end connecting to this node.
     */
    def edges: ExtSet[EdgeT]
    /** Synonym for `edges`. */  
    @inline final def ~ = edges
    
    /**
     * All edges connecting this node with `other` including outgoing and incoming edges.
     * This method is useful in case of multigraphs.
     *
     * @param other A node which is possibly connected with this node. 
     * @return All edges connecting this node with `other`.
     *         If `other` equals this node all hooks are returned.
     *         If `other` is not connected with this node an empty set is returned.   
     */
    def connectionsWith(other: NodeT): Set[EdgeT]

    /**
     * Checks whether this node has only hooks or no edges at all.
     *  
     * @return `true` if this node has only hooks or it isolated.  
     */
    def hasOnlyHooks: Boolean
    /**
     * Whether `that` is an adjacent (direct successor) to this node.
     * 
     * @param that The node to check for adjacency.
     * @return `true` if `that` is adjacent to this node.
     */
    def isDirectPredecessorOf(that: NodeT): Boolean
    /**
     * Whether `that` is independent of this node meaning that
     * there exists no edge connecting this node with `that`. 
     * 
     * @param that The node to check for independency.
     * @return `true` if `that` node is independent of this node.
     */
    def isIndependentOf(that: NodeT): Boolean
    /**
     * All direct successors of this node, also called ''successor set'' or
     * ''open out-neighborhood'': target nodes of directed incident edges and / or
     * adjacent nodes of undirected incident edges excluding this node.
     *
     * @return set of all direct successors of this node.
     */
    def diSuccessors: Set[NodeT]
    protected[collection] def addDiSuccessors(edge: EdgeT,
                                              add: (NodeT) => Unit): Unit
    /** Synonym for `diSuccessors`. */
    @inline final def outNeighbors = diSuccessors 
    /** Synonym for `diSuccessors`. */
    @inline final def ~>| = diSuccessors 
    /**
     * All direct predecessors of this node, also called ''predecessor set'' or
     * ''open in-neighborhood'': source nodes of directed incident edges and / or
     * adjacent nodes of undirected incident edges excluding this node.
     *
     * @return set of all direct predecessors of this node.
     */
    def diPredecessors: Set[NodeT]
    protected[collection] def addDiPredecessors(edge: EdgeT,
                                                add: (NodeT) => Unit)

    /** Synonym for `diPredecessors`. */
    @inline final def inNeighbors = diPredecessors 
    /** Synonym for `diPredecessors`. */
    @inline final def <~| = diPredecessors 

    /**
     * All adjacent nodes (direct successors and predecessors) of this node,
     * also called ''open neighborhood'' excluding this node.
     *
     * @return set of all neighbors.
     */
    def neighbors: Set[NodeT]
    protected[collection] def addNeighbors(edge: EdgeT,
                                           add: (NodeT) => Unit)
    
    /** Synonym for `neighbors`. */
    @inline final def ~| = neighbors 

    /**
     * All edges outgoing from this node.
     *
     * @return set of all edges outgoing from this node
     *         including undirected edges and hooks.  
     */
    def outgoing: Set[EdgeT]

    /** Synonym for `outgoing`. */
    @inline final def ~> = outgoing

    /**
     * All outgoing edges connecting this node with `to`.
     *
     * @param to The node which is the end point of zero, one or more edges starting at this node. 
     * @return All edges connecting this node with `to`.
     *         If `to` equals this node all hooks are returned.
     *         If `to` is not an adjacent an empty set is returned.   
     */
    def outgoingTo(to: NodeT): Set[EdgeT]

    /** Synonym for `outgoingTo`. */
    @inline final def ~>(to: NodeT) = outgoingTo(to)
    
    /**
     * An outgoing edge connecting this node with `to`.
     *
     * @param to The node which is the end point of an edge starting at this node. 
     * @return One of possibly several edges connecting this node with `to`.
     *         If `to` equals this node a hook may be returned.
     *         If `to` is not an adjacent node `None` is returned.   
     */
    def findOutgoingTo(to: NodeT): Option[EdgeT]

    /** Synonym for `findOutgoingTo`. */
    @inline final def ~>?(to: NodeT) = findOutgoingTo(to) 
    /**
     * Incoming edges of this node.
     *
     * @return set of all edges incoming to of this including undirected edges.  
     */
    def incoming: Set[EdgeT]

    /** Synonym for `incoming`. */
    @inline final def <~ = incoming

    /**
     * All incoming edges connecting `from` with this node.
     *
     * @param from The node with zero, one or more edges
     *             having this node as a direct successor. 
     * @return All edges at `from` having this node as a direct successor.
     *         If `from` equals this node all hooks are returned.
     *         If `from` is not an adjacent node an empty set is returned.   
     */
    def incomingFrom(from: NodeT): Set[EdgeT]

    /** Synonym for `incomingFrom`. */
    @inline final def <~(from: NodeT) = incomingFrom(from)

    /**
     * An edge at `from` having this node as a successor.
     *
     * @param from The node being at an edge which has
     *           this node as a successor. 
     * @return An edges at `from` having this node as a successor.
     *         If `from` equals this node a hook may be returned.
     *         If `from` is not an adjacent node `None` is returned.   
     */
    def findIncomingFrom(from: NodeT): Option[EdgeT]

    /** Synonym for `findIncomingFrom`. */
    @inline final def <~?(from: NodeT) = findIncomingFrom(from)

    /**
     * The degree of this node.
     *
     * @return the number of edges that connect to this node. An edge that connects
     *         to this node at more than one ends (loop) is counted as much times as
     *         it is connected to this node.
     */
    def degree: Int = {
      var cnt: Int = 0
      edges foreach { e => cnt += e.nodes.count(_ eq this) }
      cnt
    }
    /** Returns whether this node's degree equals to 0.
     */
    @inline final def isIsolated = degree == 0
    /** Returns whether this node's degree equals to 1.
     */
    @inline final def isLeaf = degree == 1
    /**
     * The outgoing degree of this node.
     *
     * @return the number of edges that go out from this node including undirected edges.
     *         Every loop on this node is counted twice. 
     */
    def outDegree: Int = {
      edges count (_.hasSource((n: NodeT) => n eq this))
    }
    /**
     * The incoming degree of this node.
     *
     * @return the number of edges that come in to this node including undirected edges.
     *         Every loop on this node is counted twice. 
     */
    def inDegree: Int = {
      edges count (_.hasTarget((n: NodeT) => n eq this))
    }
    def canEqual(that: Any) = true
    override def equals(other: Any) = other match {
      case that: GraphBase[N,E]#InnerNode => 
        (this eq that) || (that canEqual this) && (this.value == that.value)
      case thatN: N => 
        thatN match { case thatR: AnyRef => val thisN = this.value.asInstanceOf[AnyRef]  
                                           (thisN eq thatR) || (thisN == thatR)
                      case thatV => this.value == thatV }
      case _ => false
    }
    override def hashCode = value.##
  }
  object InnerNode {
    def unapply(node: InnerNode): Option[NodeT] =
      if (node isContaining selfGraph) Some(node.asInstanceOf[NodeT])
      else None
  }
  object Node {
    def apply  (node: N) = newNode(node)
    def unapply(n: NodeT) = Some(n)

    @inline final protected[collection]
    def addDiSuccessors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addDiSuccessors(edge, add)
    }
    @inline final protected[collection]
    def addDiPredecessors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addDiPredecessors(edge, add)
    }
    @inline final protected[collection]
    def addNeighbors(node: NodeT, edge: EdgeT, add: (NodeT) => Unit) {
      node.addNeighbors(edge, add)
    }
    /**
     * Allows to call methods of N directly on Node instances.
     * 
     * @param node
     * @return the contained user Object
     */
    @inline final implicit def toValue[N](node: NodeT) = node.value
  }
  protected abstract class NodeBase extends InnerNode
  protected def newNode(n: N): NodeT

  /** Base trait for graph `Ordering`s. */
  protected sealed trait ElemOrdering
  /** The empty ElemOrdering. */
  sealed class NoOrdering extends ElemOrdering
  @transient final val noOrdering = new NoOrdering
  /** Ordering for the path dependent type NodeT. */
  sealed trait NodeOrdering extends Ordering[NodeT] with ElemOrdering
  object NodeOrdering {
    /** Creates a new NodeOrdering with `compare` calling the supplied `cmp`. */
    def apply(cmp: (NodeT, NodeT) => Int) = new NodeOrdering {
      def compare(a: NodeT, b: NodeT): Int = cmp(a, b)
    }
  }
  sealed trait EdgeOrdering extends Ordering[EdgeT] with ElemOrdering
  /** Ordering for the path dependent type EdgeT. */
  object EdgeOrdering {
    /** Creates a new EdgeOrdering with `compare` calling the supplied `cmp`. */
    def apply(cmp: (EdgeT, EdgeT) => Int) = new EdgeOrdering {
      def compare(a: EdgeT, b: EdgeT): Int = cmp(a, b)
    }
  }

  @transient final protected lazy val anyOrdering = new AnyOrdering[N]
  @transient final lazy val defaultNodeOrdering = NodeOrdering(
    (a: NodeT, b: NodeT) => anyOrdering.compare(a.value, b.value)
  )
  type NodeSetT <: NodeSet
  trait NodeSet extends Set[NodeT] with ExtSetMethods[NodeT] with Serializable {
    /**
     * This method is called by the primary constructor. It must be defined by the trait
     * responsible for the implementation of the graph representation.
     *  
     * @param nodes $INNODES
     * @param edges $INEDGES
     */
    protected[collection] def initialize(nodes: Iterable[N],
                                         edges: Iterable[E[N]]): Unit
    override def stringPrefix: String = "NodeSet"
    /**
     * Sorts all nodes according to `ord` and concatenates them using `separator`. 
     * 
     * @param separator to separate nodes by.
     * @param ord custom ordering. 
     * @return sorted and concatenated string representation of this node set.
     */
    def asSortedString (separator: String = GraphBase.defaultSeparator)(
                        implicit ord: NodeOrdering = defaultNodeOrdering) =
      toList.sorted(ord) mkString separator
    /**
     * Sorts all nodes according to `ord`, concatenates them using `separator` 
     * and prefixes and parenthesizes the result with `stringPrefix`.  
     * 
     * @param separator to separate nodes by.
     * @param ord custom ordering. 
     * @return sorted, concatenated and prefixed string representation of this node set.
     */
    def toSortedString (separator: String = GraphBase.defaultSeparator)(
                        implicit ord: NodeOrdering = defaultNodeOrdering) =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"
    /**
     * Converts this node set to a set of outer nodes.
     */
    def toOuter: Set[N] = this map (n => n.value)
    @deprecated("Use toOuter instead", "1.8.0") def toNodeInSet = toOuter

    /**
     * Finds the inner node corresponding to `outerNode`.
     *
     * @return the inner node wrapped by `Some` if found, otherwise None. 
     */
    def find(outerNode: N): Option[NodeT]
    /**
     * Finds the inner node corresponding to `outerNode`.
     *
     * @return the inner node if found, otherwise `NoSuchElementException` is thrown. 
     */
    def get(outerNode: N): NodeT
    /**
     * Finds the inner node corresponding to `outerNode`.
     *
     * @return the inner node if found, otherwise `null`. 
     */
    def lookup(outerNode: N): NodeT
    def adjacencyListsToString = {
      (for(n <- this) yield
        (n.value.toString + ": " + 
        ((for (a <- n.diSuccessors) yield a.value) mkString ","))) mkString "\n"
    }
    def draw(random: Random): NodeT
  }
  /**
   * The node (vertex) set of this `Graph` commonly referred to as V(G).
   * 
   * @return Set of all contained nodes.
   */
  def nodes: NodeSetT

  type EdgeT <: InnerEdgeParam[N,E,NodeT,E] with InnerEdge with Serializable
  object EdgeT {
    def unapply(e: EdgeT): Option[(NodeT, NodeT)] = Some((e.edge._1, e.edge._2))
  }
  trait Edge extends Serializable
  trait InnerEdge
      extends /*Iterable[NodeT] with*/ InnerEdgeParam[N,E,NodeT,E] with Edge with InnerElem {
    
    override def stringPrefix = super[InnerEdgeParam].stringPrefix
    
    /**
     * The outer edge after transformation by means of the `copy` method.
     * This edge contains references to inner nodes while the original outer
     * edge contained references to outer nodes.
     */
    def edge: E[NodeT] with EdgeCopy[E]
    /**
     * The inner nodes incident with this inner edge.
     * This is just a synonym to `this` that extends `Iterable[NodeT]`.
     */
    @inline final def nodes: Iterable[NodeT] = edge.nodes
    /**
     * Finds nodes of this edge which only participate in this edge.
     * 
     * @return those nodes of this edge which do not participate in any other edge 
     */
    def privateNodes: Set[NodeT] = nodes.filter(_.edges.size == 1).toSet
    /**
     * All connecting edges, that is all edges at any of the nodes incident with this edge.
     *
     * @return set of connecting edges including hooks.
     */
    def adjacents: Set[EdgeT] = {
      var a = ArraySet[EdgeT]()
      nodes foreach {n => a ++= n.edges}
      a -= a.find(_ == edge).get
    }
    /** Synonym for `adjacents`. */
    @inline final def ~~ = adjacents
    // /**
    //  * The head (target node) of a directed edge or `_2` otherwise.
    //  */
    //  /* With 2.10, 'edge.to' can no more be called implicitly because of the addition of
    //  * 'to' to GenTraversableOnce in the standard library. So we must delegate the call. */
    // def to: NodeT = edge match {
    //   case di: DiHyperEdgeLike[NodeT] => di.to
    //   case unDi @ _                   => unDi.edge._2
    // }

    def canEqual(that: Any) = that.isInstanceOf[GraphBase[N,E]#InnerEdge] ||
                              that.isInstanceOf[EdgeLike[_]]
    override def equals(other: Any) = other match {
      case that: GraphBase[N,E]#InnerEdge => (this eq that) ||
                                                 (this.edge eq that.edge) ||
                                                 (this.edge == that.edge)
      case that: EdgeLike[_] => (this.edge eq that) ||
                                (this.edge == that)
      case _ => false
    }
    override def hashCode = edge.##
    override def toString = edge.toString
    /** Reconstructs the outer edge by means of the `copy` method. */
    def toOuter: E[N] with EdgeCopy[E] = edge.copy[N](nodes.map(n => n.value))
    //   val newNs: Iterable[N] = (edge.arity: @scala.annotation.switch) match {
    //     case 2 => Tuple2(edge._1.value, edge._2.value)
    //     case 3 => Tuple3(edge._1.value, edge._2.value, edge._n(2).value)
    //     case 4 => Tuple4(edge._1.value, edge._2.value, edge._n(2).value, edge._n(3).value)
    //     case 5 => Tuple5(edge._1.value, edge._2.value, edge._n(2).value, edge._n(3).value, edge._n(4).value)
    //     case _ => edge.nodes.map(n => n.value).toList
    //   }
    //   edge.copy[N](newNs) //.asInstanceOf[E[N]]
    // }
    @deprecated("Use toOuter instead", "1.8.0") def toEdgeIn = toOuter
  }
  object InnerEdge {
    def unapply(edge: InnerEdge): Option[EdgeT] =
      if (edge.edge._1 isContaining selfGraph) Some(edge.asInstanceOf[EdgeT])
      else None
  }
  object Edge {
    def apply(innerEdge: E[NodeT] with EdgeCopy[E]) = newEdge(innerEdge)
    def unapply(e: EdgeT) = Some(e)

    protected var freshNodes = Map[N,NodeT]()
    protected def mkNode(n: N): NodeT = {
      val existing = nodes lookup n
      if (null == existing)
        freshNodes getOrElse (n, {
          val newN = newNode(n)
          freshNodes += (n -> newN)
          newN })
      else existing
    }
    def mkNode(n: NodeParam[N]): NodeT = n match {
      case n: OuterNode[N]  => mkNode(n.value)
      case n: InnerNodeParam[N] => n.toNodeT[N,E,selfGraph.type](selfGraph)(n => mkNode(n.value))
    } 
    /**
     * Creates a new inner edge from the outer `edge` using its
     * factory method `copy` without modifying the node or edge set. */
    protected[GraphBase] def edgeToEdgeCont(edge: E[N] with EdgeCopy[E]): E[NodeT] with EdgeCopy[E] = {
      // val newNodes = (edge.arity: @scala.annotation.switch) match {
      //   case 2 => Tuple2(mkNode(edge._1), mkNode(edge._2))
      //   case 3 => Tuple3(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)))
      //   case 4 => Tuple4(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)), mkNode(edge._n(3)))
      //   case 5 => Tuple5(mkNode(edge._1), mkNode(edge._2), mkNode(edge._n(2)), mkNode(edge._n(3)), mkNode(edge._n(4)))
      //   case _ => edge.nodes.map(n => mkNode(n)).toList
      // }
      freshNodes = Map.empty[N,NodeT]
      // edge.copy[NodeT](newNodes).asInstanceOf[E[NodeT]]
      edge.copy[NodeT](edge.nodes.map(n => mkNode(n)))
    }
    // def mkNodes(node_1: N, node_2: N, nodes: N*): Product =
    // {
    //   val (n_1, n_2) = (mkNode(node_1), mkNode(node_2))
    //   val newNodes =
    //     if   (nodes.isEmpty) Tuple2(n_1, n_2)
    //     else (nodes.size: @scala.annotation.switch) match {
    //       case 1 => Tuple3(n_1, n_2, mkNode(nodes(0)))
    //       case 2 => Tuple4(n_1, n_2, mkNode(nodes(0)), mkNode(nodes(1)))
    //       case 3 => Tuple5(n_1, n_2, mkNode(nodes(0)), mkNode(nodes(1)), mkNode(nodes(2)))
    //       case _ => (nodes.map(n => mkNode(n)).toList).:::(List(n_1, n_2))
    //     }
    //   freshNodes = Map.empty[N,NodeT]
    //   newNodes
    // }
    @inline final implicit def innerEdgeToEdgeCont(edge: EdgeT): E[NodeT] = edge.edge

    def defaultWeight(edge: EdgeT) = edge.weight
    def weightOrdering[T: Numeric](weight: EdgeT => T): Ordering[EdgeT]  = {
      Ordering by weight
    }
    object WeightOrdering extends Ordering[EdgeT] {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.weight compare e2.weight
    }
    object ArityOrdering extends Ordering[EdgeT] {
      def compare(e1: EdgeT, e2: EdgeT): Int = e1.arity compare e2.arity
    }
  }
  class EdgeBase(override val edge: E[NodeT] with EdgeCopy[E])
      extends InnerEdgeParam[N,E,NodeT,E] with InnerEdge {
    // override def iterator: Iterator[NodeT] = edge.iterator.asInstanceOf[Iterator[NodeT]]
    override def stringPrefix = super.stringPrefix
  }
  protected def newEdge(innerEdge: E[NodeT] with EdgeCopy[E]): EdgeT
  @inline final protected implicit def edgeToEdgeCont(e: E[N] with EdgeCopy[E])
      : E[NodeT] with EdgeCopy[E] = Edge.edgeToEdgeCont(e)

  @transient final lazy val defaultEdgeOrdering = EdgeOrdering (
    (a: EdgeT, b: EdgeT) => {
      val unequal = (a.edge.nodes zip b.edge.nodes) find (z => z._1 != z._2)
      unequal map (t => anyOrdering.compare(t._1.value, t._2.value)) getOrElse
                  Ordering.Int.compare(a.arity, b.arity)
    }
  )
  type EdgeSetT <: EdgeSet
  trait EdgeSet extends Set[EdgeT] with ExtSetMethods[EdgeT] with Serializable {
    /**
     * This method is called by the primary constructor. It must be defined by the trait
     * responsible for the implementation of the graph representation.
     *  
     * @param edges $INEDGES
     */
    protected[collection] def initialize(edges: Iterable[E[N] with EdgeCopy[E]]): Unit
    def contains(node: NodeT): Boolean
    override def stringPrefix: String = "EdgeSet"
    /**
     * Sorts all edges according to `ord` and concatenates them using `separator`. 
     * 
     * @param separator to separate edges by.
     * @param ord custom ordering. 
     * @return sorted and concatenated string representation of this edge set.
     */
    def asSortedString (separator: String = GraphBase.defaultSeparator)(
                        implicit ord: EdgeOrdering = defaultEdgeOrdering) =
      toList.sorted(ord) mkString separator
    /**
     * Sorts all edges according to `ord`, concatenates them using `separator` 
     * and prefixes and parenthesizes the result with `stringPrefix`.  
     * 
     * @param separator to separate edges by.
     * @param ord custom ordering. 
     * @return sorted, concatenated and prefixed string representation of this edge set.
     */
    def toSortedString (separator: String = GraphBase.defaultSeparator)(
                        implicit ord: EdgeOrdering = defaultEdgeOrdering) =
      stringPrefix + "(" + asSortedString(separator)(ord) + ")"
    /**
     * Finds the inner edge corresponding to `outerEdge`.
     *
     * @return the inner node wrapped by `Some` if found, otherwise None. 
     */
    def find(outerEdge: E[N]): Option[EdgeT]

    /** The maximum arity of all edges in this edge set. */
    def maxArity: Int = if (size == 0) 0 else max(Edge.ArityOrdering).arity
    /**
     * Converts this edge set to a set of outer edges.
     */
    def toOuter: Set[E[N] with EdgeCopy[E]] = (this map (_.toOuter))
    @deprecated("Use toOuter instead", "1.8.0") def toEdgeInSet = toOuter
    
    final def draw(random: Random) = (nodes draw random).edges draw random
    final def findEntry[B](other: B, correspond: (EdgeT, B) => Boolean) = {
      def find(edge: E[N]): EdgeT = correspond match {
        case c: ((EdgeT, E[N]) => Boolean) => nodes.lookup(edge._1).edges findEntry (edge, c) 
        case _ => throw new IllegalArgumentException
      } 
      other match {
        case e: OuterEdge[N,E]      => find(e.edge)
        case e: InnerEdgeParam[N,E,_,E] => find(e.asEdgeT[N,E,selfGraph.type](selfGraph).toOuter)
        case _                   => null.asInstanceOf[EdgeT]
      }
    }
  }
  /**
   * The edge set of this `Graph` commonly referred to as E(G).
   * 
   * @return Set of all contained edges.
   */
  def edges: EdgeSetT
  def totalWeight = (0L /: edges)(_ + _.weight)
}
object GraphBase {
  val defaultSeparator = ", "
}
