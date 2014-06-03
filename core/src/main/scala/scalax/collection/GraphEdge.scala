package scalax.collection

import language.{higherKinds, postfixOps}
import scala.annotation.{tailrec, switch}
import scala.collection.mutable.Set

import GraphPredef.{InnerNodeParam, OuterEdge}
import edge.LBase.LEdge

/**
 * This object serves as a container for all edge-types to be used in the context of `Graph`.
 * You will usually simply import all its members along with the members of Param:
 * {{{
 * import scalax.collection.GraphPredef._, scalax.collection.GraphEdge,_
 * }}}
 * @define SHORTCUT Allows to replace the edge object with it's shortcut like
 * @author Peter Empen
 */
object GraphEdge {
  /**
   * Template for Edges in a Graph.
   * 
   * Implementation note: Irrespective of the containing `Graph` all library-provided Edges
   * are immutable.
   *
   * RHZ: Should EdgeLike extend Iterable[N] now that nodes is of type Iterable[N]?
   * 
   * @tparam N the user type of the nodes (ends) of this edge.
   * @define CalledByValidate This function is called on every edge-instantiation
   *         by `validate` that throws EdgeException if this method returns `false`.
   * @define ISAT In case this edge is undirected this method maps to `isAt`
   * @author Peter Empen
   */
  sealed trait EdgeLike[+N] extends Iterable[N] with Eq with Serializable
  {
    // RHZ: Should we expose the N type parameter?
    // type Node = N
    /** The end nodes joined by this edge. */
    def nodes: Iterable[N]
    /** Iterator for the nodes (end-points) of this edge.
     */
    def iterator: Iterator[N]
    /** Sequence of the end points of this edge.
     */
    def nodeSeq: Seq[N] = iterator.toSeq
    /** The first node. Same as _n(0). */
    def _1: N = nodes.head
    /** The second node. Same as _n(1). */
    def _2: N =  nodes.tail.head
    /** The n'th node with 0 <= n < arity. */
    def _n(n: Int): N = (n: @switch) match {
      case 0 => _1
      case 1 => _2
      case _ => {
        if (n < 0 || n >= arity)
          throw new IndexOutOfBoundsException
        nodes.drop(n).head
      }  
    }
    /**
     * Number of nodes linked by this Edge. At least two nodes are linked. In case of
     * a hook, the two nodes are identical. Hyper-edges may link more than two nodes.
     */
    def arity = nodes.size
    /**
     * A function to determine whether the `arity` of the passed `Product`
     * of nodes (that is the number of edge ends) is valid.
     * $CalledByValidate
     *
     * RHZ: When would this method be called with a value different than `this.arity`?
     *      Does it really need to have a parameter?
     */
    protected def isValidArity(size: Int): Boolean = size >= 2
    /**
     * This method may be overridden to enforce additional validation at edge
     * creation time. Be careful to call `super.isValidCustom` when overriding.
     * $CalledByValidate
     */
    protected def isValidCustom = true
    protected def isValidCustomExceptionMessage = "Custom validation failed: " + toString
    /**
     * Performs basic, inevitable edge validation. Among others, ensures
     * that `nodes ne null` and no edge end `eq null`.
     * 
     * This validation method must be called in the constructor of any edge class
     * that directly extends or mixes in `EdgeLike`. To perform additional custom
     * validation `isValidCustom` is to be overridden.
     *  
     *  @throws EdgeException if any of the basic validations or of eventually
     *  supplied additional validations fails.
     */
    protected final def validate {
      if(nodes eq null)
        throw new EdgeException(s"null node in: $toString")
      val ar = arity
      if (! isValidArity(ar))
        throw new EdgeException(s"Invalid arity: $ar in: $toString")
      if (! isValidCustom)
        throw new EdgeException(isValidCustomExceptionMessage)
    }
    /** `true` it this edge is directed. */
    def directed = false
    /** Same as `directed`. */
    @inline final def isDirected = directed
    /** `true` it this edge is undirected. */
    @inline final def undirected = ! directed
    /** Same as `undirected`. */
    @inline final def isUndirected = undirected
    /** `true` if this is a hyperedge that is it may have more than two ends. */
    def isHyperEdge = true
    /** `true` if this edge has exactly two ends. */
    @inline final def nonHyperEdge = ! isHyperEdge
    /** `true` if this edge produces a self-loop.
     * In case of a non-hyperedge, a loop is given if the incident nodes are equal.
     * In case of a directed hyperedge, a loop is given if the source is equal to
     * any of the targets.
     * In case of an undirected hyperedge, a loop is given if any pair of incident
     * nodes has equal nodes.
     */
    def isLooping = if (arity == 2) _1 == _2
                    else if (directed) iterator.drop(1) exists (_ == _1)
                    else (Set() ++= iterator).size < arity
    /** Same as `! looping`. */                
    final def nonLooping = ! isLooping 
    /**
     * The weight of this edge defaulting to 1L.
     * 
     * Note that `weight` is normally not part of the edge key (hashCode) meaning that
     * edges of different weights connecting the same nodes will be treated as equal
     * and not added more than once. If you need multi-edges based on different weights 
     * you should either make use of a predefined key-weighted edge type such as `WDiEdge` 
     * or define a custom edge class that mixes in `ExtendedKey` and adds `weight` to
     * `keyAttributes`. 
     * 
     * In case of weights of a type other than `Long` you either convert values of
     * that type to `Long` prior to edge creation or define a custom edge class 
     * to include your own `val` of that type and override `def weight` to provide
     * a conversion.  
     */
    def weight: Long = 1
    /**
     * The label of this edge. If `Graph`'s edge type parameter has been inferred or set
     * to a labeled edge type all contained edges are labeled. Otherwise you should
     * assert, for instance by calling `isLabeled`, that the edge instance is labeled
     * before calling this method.
     * 
     * Note that `label` is normally not part of the edge key (hashCode) meaning that
     * edges of different labels connecting the same nodes will be treated as equal
     * and not added more than once. If you need multi-edges based on different labels 
     * you should either make use of a predefined key-labeled edge type such as `LDiEdge` 
     * or define a custom edge class that mixes in `ExtendedKey` and adds `label` to
     * `keyAttributes`. 
     * 
     * @throws UnsupportedOperationException if the edge is non-labeled.
     */
    def label: Any =
      throw new UnsupportedOperationException("Call of label for a non-labeled edge.")
    /** `true` if this edge is labeled. See also `label`. */
    def isLabeled = this.isInstanceOf[LEdge[N]]

    /** Same as `isAt`. */
    @inline final def contains[M>:N](node: M): Boolean = isAt(node)

    /** `true` if `node` is incident with this edge. */
    def isAt[M>:N](node: M): Boolean
    /** `true` if any end of this edge fulfills `pred`. */
    def isAt(pred: N => Boolean): Boolean
    
    /** `true` if `node` is a source of this edge. $ISAT. */
    def hasSource[M>:N](node: M): Boolean
    /** `true` if any source end of this edge fulfills `pred`. */
    def hasSource(pred: N => Boolean): Boolean

    /** `true` if `node` is a target of this edge. $ISAT. */
    def hasTarget[M>:N](node: M): Boolean
    /** `true` if any target end of this edge fulfills `pred`. */
    def hasTarget(pred: N => Boolean): Boolean
    
    /** Applies `f` to the source end resp. to all source ends of this edge. */
    def withSources(f: N => Unit): Unit
    /** Applies `f` to the target end resp. to all target ends of this edge. */
    def withTargets(f: N => Unit): Unit

    /** `true` if<br />
     *  a) both `n1` and `n2` are at this edge for an undirected edge<br />
     *  b) `n1` is a source and `n2` a target of this edge for a directed edge. */
    def matches[M>:N](n1: M, n2: M): Boolean
    /** `true` if<br />
     *  a) two distinct ends of this undirected edge exist
     *     for which `p1` and `p2` hold or<br />
     *  b) `p1` holds for a source and `p2` for a target of this directed edge. */
    def matches(p1: N => Boolean, p2: N => Boolean): Boolean

    override def canEqual(that: Any): Boolean = that.isInstanceOf[EdgeLike[_]]

    override def equals(other: Any): Boolean = other match {
      case that: EdgeLike[_] => 
        (this eq that) ||
        (that canEqual this) &&
        (this.directed == that.directed) &&
        (this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed]) &&
        equals(that)
      case _ => false
    }

    /** Preconditions:
     *  `this.directed == that.directed &&`
     *  `this.isInstanceOf[Keyed] == that.isInstanceOf[Keyed]`
     */
    protected def equals(other: EdgeLike[_]): Boolean = baseEquals(other)
    
    override def hashCode: Int = baseHashCode
  
    final protected def thisSimpleClassName = try {
      this.getClass.getSimpleName
    } catch { // Malformed class name
      case e: java.lang.InternalError => this.getClass.getName
    }
    override def stringPrefix = "Nodes"
    protected def nodesToStringWithParenthesis = false
    protected def nodesToStringSeparator = EdgeLike.nodeSeparator
    protected def nodesToString =
      if (nodesToStringWithParenthesis)
        nodes.toString.patch(0, stringPrefix, nodes.stringPrefix.length)
      else nodes mkString nodesToStringSeparator
    protected def attributesToString = ""
    protected def toStringWithParenthesis = false
    protected def brackets = EdgeLike.curlyBraces
    override def toString = {
      val attr = attributesToString
      val woParenthesis = nodesToString + ( if(attr.length > 0) attr else "" )
      if (toStringWithParenthesis)
        thisSimpleClassName + brackets.left + woParenthesis + brackets.right
      else
        woParenthesis
    }
  }
  /**
   * This trait is to be mixed in by every class implementing EdgeLike.
   */
  trait EdgeCopy[+CC[X] <: EdgeLike[_]] {
    /**
     * It is a prerequisite for edge-classes to implement this method. Otherwise
     * they cannot be passed to a `Graph`.
     * 
     * `Graph` calls this method internally to obtain a new instance of the 
     * edge passed to `Graph` with nodes of the type of the inner class `NodeT`
     * which itself contains the outer node. 
     */
    // RHZ: Maybe Iterator[NN] or TraversableOnce[NN] would be more appropiate
    protected[collection] def copy[NN](newNodes: Iterable[NN]): CC[NN] with EdgeCopy[CC]
  }
  object EdgeLike {
    val nodeSeparator = "~" 
    protected case class Brackets(val left: Char, val right: Char)
    protected val curlyBraces = Brackets('{', '}')
    def unapply[N](e: EdgeLike[N]) = Some(e)
  }
  // /**
  //  * Helper object to convert edge-factory parameter-lists to tuple-n or list.
  //  *
  //  * @author Peter Empen
  //  */
  // object NodeProduct {
  //   @inline final def apply[N](node_1: N, node_2: N)     : Product = Tuple2(node_1, node_2)
  //   @inline def apply[N](node_1: N, node_2: N, nodes: N*): Product = {
  //     (nodes.size: @switch) match {
  //       case 1 => Tuple3(node_1, node_2, nodes(0))
  //       case 2 => Tuple4(node_1, node_2, nodes(0), nodes(1))
  //       case 3 => Tuple5(node_1, node_2, nodes(0), nodes(1), nodes(2))
  //       case _ => List[N](node_1, node_2) ::: List(nodes: _*)
  //     }
  //   }
  //   final def apply[N](nodes: Iterable[N]): Product = nodes match {
  //     case n1 :: n2 :: rest =>
  //       if (rest eq Nil) apply(n1, n2)
  //       else             apply(n1, n2, rest: _*)
  //     case _ =>
  //       val it = nodes.iterator
  //       val n1 = it.next
  //       val n2 = it.next
  //       if (it.hasNext) apply(n1, n2, it.toList: _*)
  //       else            apply(n1, n2)
  //   }
  // }
  protected[collection] trait Keyed
  // RHZ: Why should there be a separate `Eq` trait, why not just add
  // these methods to `EdgeLike`?
  protected[collection] sealed trait Eq {
    protected def baseEquals(other: EdgeLike[_]): Boolean
    protected def baseHashCode: Int
    final protected def nrEqualingNodes(itA: Iterator[_], itB: Iterable[_]): Int = {
      var nr = 0
      for (a <- itA; b <- itB)
        if (a == b) nr += 1
      nr
    }
  }
  protected[collection] trait EqHyper extends Eq {
    this: EdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) = {
      val thisArity = this.arity
      thisArity == other.arity &&
      thisArity == nrEqualingNodes(this.iterator, other)
    }

    override protected def baseHashCode: Int = (0 /: nodes)(_ ^ _.hashCode)
  }
  protected[collection] trait EqDiHyper extends Eq {
    this: DiHyperEdgeLike[_] =>

    override protected def baseEquals(other: EdgeLike[_]) = (arity: @switch) match {
      case 2 => other.arity == 2 &&
                this._1 == other._1 &&
                this._2 == other._2
      case a => other.arity == a && 
               (other match {
                  case diHyper: DiHyperEdge[_] =>
                    this.source == diHyper.source &&
                    nrEqualingNodes(this.targets, Set() ++ diHyper.targets) == a - 1
                  case _ => throw new IllegalArgumentException("Unexpected edge type.")
                })
    }
    
    override protected def baseHashCode = {
      var m = 4
      def mul(i: Int): Int = { m += 3; m * i }
      (0 /: nodes)((s: Int, n: Any) => s ^ mul(n.hashCode))
    }
  }
  protected[collection] trait EqUnDi extends Eq {
    this: EdgeLike[_] =>

    @inline final protected def unDiBaseEquals(n1: Any, n2: Any) =
      (this._1 == n1 && this._2 == n2 ||
       this._1 == n2 && this._2 == n1)

    override protected def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 && unDiBaseEquals(other._1, other._2)

    override protected def baseHashCode = (_1 ##) ^ (_2 ##)
  }
  protected[collection] trait EqDi extends Eq {
    this: DiEdgeLike[_] =>

    @inline final protected def diBaseEquals(n1: Any, n2: Any) =
      this._1 == n1 &&
      this._2 == n2
      
    final protected override def baseEquals(other: EdgeLike[_]) =
      other.arity == 2 && diBaseEquals(other._1, other._2)

    override protected def baseHashCode = (23 * (_1 ##)) ^ (_2 ##)
  }
  /**
   * Template trait for directed edges.
   * 
   * Any class representing directed edges must inherit from this trait.
   * 
   * @author Peter Empen
   */
  trait DiHyperEdgeLike[+N]
      extends EdgeLike[N]
      with    EqDiHyper
  {
    @inline final override def directed = true

    /** Synonym for `source`. */
    @inline final def from = source
    /** The single source node of this directed edge. */
    @inline final def source = _1
    
    /** Synonym for `target`. */
    def to = target
    /** The target node for a directed edge;
     *  one of the target nodes for a directed hyperedge. */
    @inline final def target = _2

    /** The target nodes of this directed edgeor hyperedge. */
    @inline final def targets = iterator drop 1

    override def hasSource[M>:N](node: M) = this._1 == node
    override def hasSource(pred: N => Boolean) = pred(this._1)

    override def hasTarget[M>:N](node: M) = targets contains node
    override def hasTarget(pred: N => Boolean) = targets exists pred

    override def withSources(f: N => Unit) = f(this._1)
    override def withTargets(f: N => Unit) = targets foreach f

    override def matches[M >: N](n1: M, n2: M): Boolean =
      source == n1 && (targets exists (_ == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(source) && (targets exists p2)

    override protected def nodesToStringSeparator = DiEdgeLike.nodeSeparator
  }
  trait DiEdgeLike[+N]
      extends DiHyperEdgeLike[N]
      with    EqDi {
    @inline final override def to = _2

    final override def hasSource[M>:N](node: M) = this._1 == node
    final override def hasSource(pred: N => Boolean) = pred(this._1)

    final override def hasTarget[M>:N](node: M) = this._2 == node
    final override def hasTarget(pred: N => Boolean) = pred(this._2)

    final override def withSources(f: N => Unit) = f(this._1)
    final override def withTargets(f: N => Unit) = f(this._2)
    
    override def matches[M >: N](n1: M, n2: M): Boolean = diBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      p1(this._1) && p2(this._2)
  }
  object DiEdgeLike {
    val nodeSeparator = "~>" 
    def unapply[N](e: DiEdgeLike[N]) = Some(e)
  }

  case class EdgeException(val msg: String) extends Exception
  
  /**
   * This trait supports extending the default key of an edge with additional attributes.
   *  
   * As a default, the key - represented by `hashCode` - of an edge is made up of the
   * participating nodes.
   * Custom edges may need to expand this default key with additional attributes
   * thus enabling the definition of several edges between the same nodes (multi-edges).
   * Edges representing flight connections between airports are a typical example
   * for this requirement because their key must also include something like a flight number.
   * When defining a custom edge for a multi-graph, this trait can be mixed in.
   *     
   * @tparam N    type of the nodes
   * @author Peter Empen
   */
  trait ExtendedKey[N] extends EdgeLike[N]
  {
    /**
     * Each element in this sequence references an attribute of the custom  
     * edge which composes the key of this edge. All attributes added to this sequence
     * will be considered when calculating `equals` and `hashCode`.
     * 
     * Neither an empty sequence, nor null elements are permitted.
     */
    def keyAttributes: Seq[Any]
    override def equals(other: Any) =
      super.equals(other) && (other match {
        case that: ExtendedKey[_] => this.keyAttributes == that.keyAttributes  
        case _ => false
      })
    override def hashCode = super.hashCode + (keyAttributes map (_.## * 41) sum)  
  
    override protected def attributesToString: String
  }
  object ExtendedKey {
    def unapply[N](e: ExtendedKey[N]) = Some(e)
  }
  trait LoopFreeEdge[N] extends EdgeLike[N]
  {
    override protected def isValidCustom = {
      super.isValidCustom
      if (arity == 2) _1 != _2
      else nonLooping
    }
    override protected def isValidCustomExceptionMessage = "No loop is allowed: " + toString
  }
  object LoopFreeEdge {
    def unapply[N](e: LoopFreeEdge[N]) = Some(e)
  }
  /** Marker trait for companion objects of any kind of edge. */
  trait EdgeCompanionBase[+E[N] <: EdgeLike[N]] extends Serializable
  /**
   * The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) hyperedges.
   * 
   * @author Peter Empen
   */
  trait HyperEdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N, nodes: N*): E[N]
    // TODO: Remove all the `from`s and stick to `apply` for consistency
    /** @param nodes must be of arity >= 2 */
    protected[collection] def from[N](nodes: Iterable[N]): E[N]
  }
  /**
   * The abstract methods of this trait must be implemented by companion objects
   * of simple (non-weighted, non-labeled) edges.
   * 
   * @author Peter Empen
   */
  trait EdgeCompanion[+E[N] <: EdgeLike[N]] extends EdgeCompanionBase[E] {
    def apply[N](node_1: N, node_2: N): E[N]
    /** @param nodes must be of arity == 2 */
    protected[collection] def from[N](n1: N, n2: N): E[N]
  }
  // ------------------------------------------------------------------------ *
  /** Represents an undirected hyperedge (hyperlink) in a hypergraph
   *  with unlimited number of nodes.
   * 
   * @author Peter Empen
   */
  // RHZ: Why undirected edges are UnDiEdges but undirected hyperedges
  // are HyperEdge and not UnDiHyperEdge? or maybe UnDiEdge should
  // be just Edge?
  @SerialVersionUID(50L)
  class HyperEdge[N] (override val nodes: Iterable[N])
    extends EdgeLike[N]
    with    EdgeCopy[HyperEdge]
    with    OuterEdge[N,HyperEdge]
    with    EqHyper
  {
    validate

    override protected[collection]
    def copy[NN](newNodes: Iterable[NN]) = new HyperEdge[NN](newNodes)

    /** Iterator for the nodes (end-points) of this edge.
     */
    def iterator: Iterator[N] = nodes match {
      case i: Iterable[N] => i.iterator
      case p: Product   => p.productIterator.asInstanceOf[Iterator[N]] 
    }

    override def isAt[M>:N](node: M) = nodes exists (_ == node)
    override def isAt(pred: N => Boolean) = nodes exists pred
    
    override def hasSource[M>:N](node: M) = isAt(node)
    override def hasSource(pred: N => Boolean) = isAt(pred)

    override def hasTarget[M>:N](node: M) = isAt(node)
    override def hasTarget(pred: N => Boolean) = isAt(pred)

    override def withSources(f: N => Unit) = iterator foreach f
    override def withTargets(f: N => Unit) = withSources(f)

    final protected def matches(fList: Seq[N => Boolean]): Boolean = {
      val it = nodes.toIterator
      @tailrec def loop(checks: Seq[N => Boolean]): Boolean = {
        if (checks.isEmpty) true 
        else if (! it.hasNext) false
        else {
          val n = it.next
          val f = checks find (f => f(n))
          if (f.isDefined) loop(checks diff List(f.get))
          else             loop(checks)
        }
      }
      loop(fList)
    }
    override def matches[M >: N](n1: M, n2: M): Boolean =
      matches(List((n: M) => n == n1,
                   (n: M) => n == n2))
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
      matches(List(p1, p2))
  }
  /**
   * Factory for undirected hyper-edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2 ~ node_3`
   * to `HyperEdge`.
   * 
   * @author Peter Empen
   */
  object HyperEdge extends HyperEdgeCompanion[HyperEdge] {
    // Optimised hyperedges
    def apply[N](n1: N, n2: N) =
      new HyperEdge[N](Iterable(n1, n2)) {
        @inline final override def _1 = n1
        @inline final override def _2 = n2
        @inline final override def arity = 2
        @inline final override def _n(n: Int): N = (n: @switch) match {
          case 0 => n1
          case 1 => n2
          case _ => throw new IndexOutOfBoundsException
        }
      }
    def apply[N](n1: N, n2: N, n3: N) =
      new HyperEdge[N](Iterable(n1, n2, n3)) {
        @inline final override def _1 = n1
        @inline final override def _2 = n2
        @inline final override def arity = 3
        @inline final override def _n(n: Int): N = (n: @switch) match {
          case 0 => n1
          case 1 => n2
          case 2 => n3
          case _ => throw new IndexOutOfBoundsException
        }
      }
    def apply[N](n1: N, n2: N, n3: N, n4: N) =
      new HyperEdge[N](Iterable(n1, n2, n3, n4)) {
        @inline final override def _1 = n1
        @inline final override def _2 = n2
        @inline final override def arity = 4
        @inline final override def _n(n: Int): N = (n: @switch) match {
          case 0 => n1
          case 1 => n2
          case 2 => n3
          case 3 => n4
          case _ => throw new IndexOutOfBoundsException
        }
      }
    def apply[N](n1: N, n2: N, n3: N, n4: N, n5: N) =
      new HyperEdge[N](Iterable(n1, n2, n3, n4, n5)) {
        @inline final override def _1 = n1
        @inline final override def _2 = n2
        @inline final override def arity = 5
        @inline final override def _n(n: Int): N = (n: @switch) match {
          case 0 => n1
          case 1 => n2
          case 2 => n3
          case 3 => n4
          case 4 => n5
          case _ => throw new IndexOutOfBoundsException
        }
      }

    // Non-optimised hyperedges
    def apply[N](n1: N, n2: N, n3: N, n4: N, n5: N, nodes: N*) =
        new HyperEdge[N](Iterable(n1, n2, n3, n4, n5) ++ nodes)

    def from [N](nodes: Iterable[N]): HyperEdge[N] = apply(nodes)
    def apply[N](nodes: Iterable[N]): HyperEdge[N] =
      (nodes.size: @switch) match {
        case 0 => throw new IllegalArgumentException("too few nodes")
        case 1 => throw new IllegalArgumentException("too few nodes") // actually, hyperedges can be singletons no?
        case 2 => { val ns = nodes.iterator; HyperEdge(ns.next, ns.next) }
        case 3 => { val ns = nodes.iterator; HyperEdge(ns.next, ns.next, ns.next) }
        case 4 => { val ns = nodes.iterator; HyperEdge(ns.next, ns.next, ns.next, ns.next) }
        case 5 => { val ns = nodes.iterator; HyperEdge(ns.next, ns.next, ns.next, ns.next, ns.next) }
        case _ => new HyperEdge[N](nodes)
      }

    def unapplySeq[N](e: HyperEdge[N]) =
      if (e eq null) None else Some(e._1, e.nodes drop 1)
  }
  /** $SHORTCUT `hyperedge match {case n1 ~~ (n2, n3) => f(n1, n2, n3)}`.
   */
  val ~~ = HyperEdge
  
  /**
   * Represents a directed edge (link) in a hypergraph with unlimited number of nodes.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(51L)
  class DiHyperEdge[N] (val sources: Iterable[N],
                        val targets: Iterable[N])
    extends HyperEdge[N](sources ++ targets)
    with    DiHyperEdgeLike[N]
    with    EdgeCopy[DiHyperEdge]
    with    OuterEdge[N,DiHyperEdge]
  {
    override protected[collection] def copy[NN](newNodes: Iterable[NN]) = {
      require(newNodes.size == nodes.size, "too many or too few nodes for copy")
      val len = sources.size
      // create a copy with the same number of sources and targets
      new DiHyperEdge[NN](newNodes take len, newNodes drop len)
    }
  }
  /**
   * Factory for directed hyper-edges.
   * `GraphPredef` also supports implicit conversion from
   * `node_1 ~> node_2 ~> node_3` to `DiHyperEdge`.
   * 
   * @author Peter Empen
   */
  object DiHyperEdge extends DiHyperEdgeCompanion[DiHyperEdge] {
    // TODO: For this to work in the way I want I'll have to make them
    // implicit classes that take the tuple as parameter I think
    def apply[N](from: N, to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from), Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3, from._4),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3, from._4,
                               from._5),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N, N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3, from._4,
                               from._5, from._6),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N, N, N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3, from._4,
                               from._5, from._6, from._7),
                         Iterable(to_1) ++ to_n)
    def apply[N](from: (N, N, N, N, N, N, N, N), to_1: N, to_n: N*) =
      new DiHyperEdge[N](Iterable(from._1, from._2, from._3, from._4,
                               from._5, from._6, from._7, from._8),
                         Iterable(to_1) ++ to_n)

    def from [N](sources: Iterable[N], targets: Iterable[N]) =
      new DiHyperEdge[N](sources, targets)
    def apply[N](sources: Iterable[N], targets: Iterable[N]) =
      new DiHyperEdge[N](sources, targets)
    def apply[N](s1: N, sources: N*)(t1: N, targets: N*) =
      new DiHyperEdge[N](Iterable(s1) ++ sources, Iterable(t1) ++ targets)
    def unapplySeq[N](e: DiHyperEdge[N]) =
      if (e eq null) None else Some(e.sources, e.targets)
  }
  /** $SHORTCUT `diHyperedge match {case source ~~> (t1, t2) => f(source, t1, t2)}`.
   */
  val ~~> = DiHyperEdge
  
  /**
   * Represents an undirected edge.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(52L)
  class UnDiEdge[N] (override val _1: N, override val _2: N)
    extends HyperEdge[N](Iterable(_1, _2))
    with    EdgeCopy[UnDiEdge]
    with    OuterEdge[N,UnDiEdge]
    with    EqUnDi
  {
    @inline final override protected def isValidArity(size: Int) = size == 2
    @inline final override def isHyperEdge = false

    override protected[collection] def copy[NN](newNodes: Iterable[NN]) = {
      require(newNodes.size == 2, "an UnDiEdge can only link 2 nodes")
      new UnDiEdge[NN](newNodes.head, newNodes.tail.head)
    }

    @inline final override def size = 2
    override def iterator: Iterator[N] = new AbstractIterator[N] {
      private var count = 0
      def hasNext = count < 2
      def next: N = {
        count += 1
        (count: @switch) match {
          case 1 => _1
          case 2 => _2
          case _ => Iterator.empty.next
        } 
      }
    }

    @inline final override def arity = 2
    
    override def isAt[M>:N](node: M) = _1 == node || _2 == node
    override def isAt(pred: N => Boolean) = pred(_1) || pred(_2)

    override def withSources(f: N => Unit) = { f(this._1); f(this._2) }
    override def withTargets(f: N => Unit) = withSources(f)

    override def matches[M >: N](n1: M, n2: M): Boolean = unDiBaseEquals(n1, n2)
    override def matches(p1: N => Boolean, p2: N => Boolean): Boolean =
        (p1(_1) && p2(_2) ||
         p1(_2) && p2(_1)  )
  }
  /**
   * Factory for undirected edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~ node_2` to `UnDiEdge`.
   * 
   * @author Peter Empen
   */
  object UnDiEdge extends EdgeCompanion[UnDiEdge] {
    def from [N](node_1: N, node_2: N) = new UnDiEdge[N](node_1, node_2)
    def apply[N](node_1: N, node_2: N) = new UnDiEdge[N](node_1, node_2)
    def apply[N](nodes: (N, N)) = new UnDiEdge[N](nodes._1, nodes._2)
    protected[collection] def from[N](nodes: Iterable[N]) = {
      require(nodes.size == 2, "an UnDiEdge can only link 2 nodes")
      new UnDiEdge[N](nodes.head, nodes.tail.head)
    }
    def unapply[N](e: UnDiEdge[N]) = if (e eq null) None else Some(e._1, e._2)
  }
  /** $SHORTCUT `edge match {case n1 ~ n2 => f(n1, n2)}`.
   */
  val ~ = UnDiEdge
  
  /**
   * Represents a directed edge (arc / arrow) connecting two nodes.
   * 
   * @author Peter Empen
   */
  @SerialVersionUID(53L)
  class DiEdge[N] (source: N, target: N)
    extends UnDiEdge[N](source, target)
    with    DiEdgeLike[N]
    with    EdgeCopy[DiEdge]
    with    OuterEdge[N,DiEdge]
  {
    override protected[collection] def copy[NN](newNodes: Iterable[NN]) = {
      require(newNodes.size == 2, "a DiEdge can only link 2 nodes")
      new DiEdge[NN](newNodes.head, newNodes.tail.head)
    }
  }
  /**
   * Factory for directed edges.
   * `GraphPredef` also supports implicit conversion from `node_1 ~> node_2` to `DiEdge`.
   * 
   * @author Peter Empen
   */
  object DiEdge extends EdgeCompanion[DiEdge] {
    def from [N](n1: N, n2: N)   = new DiEdge[N](n1, n2)
    def apply[N](from: N, to: N) = new DiEdge[N](from, to)
    def apply[N](nodes: (N, N))  = new DiEdge[N](nodes._1, nodes._2)
    protected[collection] def from[N](nodes: Iterable[N]) = {
      require(nodes.size == 2, "a DiEdge can only link 2 nodes")
      new UnDiEdge[N](nodes.head, nodes.tail.head)
    }
    def unapply[N](e: DiEdge[N]) = if (e eq null) None else Some(e.source, e.target)
  }
  /** $SHORTCUT `edge match {case source ~> target => f(source, target)}`.
   */
  val ~> = DiEdge
}
