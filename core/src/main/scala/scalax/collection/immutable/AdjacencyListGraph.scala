package scalax.collection
package immutable

import language.higherKinds
import collection.Set

import GraphPredef.EdgeLikeIn
import GraphEdge.EdgeCopy
import mutable.ArraySet

/** Implements an incident list based immutable graph representation.
 *   
 * @author Peter Empen
 */
trait AdjacencyListGraph[N,
                         E[X] <: EdgeLikeIn[X],
                        +This[X, Y[X]<:EdgeLikeIn[X]]
                              <: AdjacencyListGraph[X,Y,This] with Graph[X,Y]]
  extends GraphLike[N,E,This]
  with    AdjacencyListBase[N,E,This]
{ selfGraph: This[N,E] =>

  type NodeT <: InnerNodeImpl 
  abstract class InnerNodeImpl(value: N, hints: ArraySet.Hints)
    extends NodeBase(value)
    with    InnerNode
  { this: NodeT =>
    
    final override val edges: ArraySet[EdgeT] = ArraySet.emptyWithHints[EdgeT](hints)
    
    import mutable.EqMap._
    @transient final lazy val diSuccessors: Set[NodeT] = Adj.diSucc.toKeySet
  }

  type NodeSetT = NodeSet
  @SerialVersionUID(7870L)
  class NodeSet extends super.NodeSet
  {
    @inline final override protected def minus(node: NodeT) { coll -= node }
    def +(node: NodeT) =
      if (coll contains node) this
      else {val c = copy; c.coll += node; c }
    
    protected[collection] def +=(edge: EdgeT): this.type = {
      edge.nodes foreach { n =>
        val inColl = coll findEntry n getOrElse {coll += n; n}
        inColl += edge
      }
      this
    }
  }
  override val nodes: NodeSetT

  type EdgeT = EdgeBase
  @inline final protected def newEdgeTArray(size: Int): Array[EdgeT] = new Array[EdgeT](size)
  @inline final override protected def newEdge(innerEdge: E[NodeT] with EdgeCopy[E]) = new EdgeT(innerEdge)
  type EdgeSetT = EdgeSet
  @SerialVersionUID(7873L)
  class EdgeSet extends super.EdgeSet
  {
    override protected[collection] def initialize(edges: Iterable[E[N] with EdgeCopy[E]]) {
      if (edges ne null)
        edges foreach (this += Edge(_))
    }
    final override lazy val size = super.size
    @inline final protected[AdjacencyListGraph] def +=(edge: EdgeT): this.type = {
      nodes += edge
      this
    }
    @inline final protected[immutable] def addEdge(edge: EdgeT) { +=(edge) }
    @inline final def +(edge: EdgeT): Set[EdgeT] = toSet + edge
    @inline final def -(edge: EdgeT): Set[EdgeT] = toSet - edge
    @inline final override lazy val maxArity = super.maxArity
  }
  override val edges: EdgeSetT

  def copy(nodes: collection.Iterable[N],
           edges: collection.Iterable[E[N] with EdgeCopy[E]]): This[N,E]
  def + (n: N) = if (nodes contains Node(n)) this
                 else copy(nodes.toOuter.toBuffer += n,
                           edges.toOuter)
  protected
  def +# (e: E[N] with EdgeCopy[E]) = if (edges contains Edge(e)) this
                                      else copy(nodes.toOuter,
                                                edges.toOuter.toBuffer += e)
  def - (n: N) = nodes find (nf => nf.value == n) match {
    case Some(nf) => copy(nodes.toOuter.toBuffer  -= n,
                          edges.toOuter.toBuffer --= (nf.edges map (_.toOuter)))
    case None     => this
  }
  def -? (n: N) = nodes find n match {
    case Some(nf) => {
      val newNodes = nodes.toOuter.toBuffer
      val newEdges = edges.toOuter.toBuffer
      nodes.subtract(nf, false,
        nf => newNodes  -= n,
        nf => newEdges --= (nf.edges map (_.toOuter)))
      copy(newNodes, newEdges)
    }
    case None => this
  }
  protected def -# (e: E[N] with EdgeCopy[E]) =
    if (edges contains Edge(e))
      copy(nodes.toOuter,
        edges.toOuter.toBuffer -= e)
    else this
  protected def -!# (e: E[N] with EdgeCopy[E]) = edges find (ef => ef == e) match {
    case Some(ef) => copy(nodes.toOuter.toBuffer --= ef.privateNodes map (n => n.value),
                          edges.toOuter.toBuffer  -= e)
    case None     => this
  }
}
