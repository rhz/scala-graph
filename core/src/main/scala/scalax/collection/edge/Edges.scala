package scalax.collection.edge

import scalax.collection.GraphEdge._,
       scalax.collection.GraphPredef._
import scalax.collection.Graph
// import WBase._, LBase._
import LBase._

// // ------------------------------------------------------------------------- W*
// /** weighted undirected edge. */
// @SerialVersionUID(72L)
// class WUnDiEdge[N](nodes: Product,
//                    override val weight: Long)
//   extends UnDiEdge[N](nodes)
//   with    WEdge   [N]
//   with    EdgeCopy[WUnDiEdge]
//   with    OuterEdge  [N,WUnDiEdge]
// {
//   override protected[collection]
//   def copy[NN](newNodes: Product) = new WUnDiEdge[NN](newNodes, weight)
// }
// object WUnDiEdge extends WEdgeCompanion[WUnDiEdge] {
//   override def newEdge[N](nodes: Product, weight: Long) = new WUnDiEdge[N](nodes, weight)
// }
// /** weighted directed edge. */
// @SerialVersionUID(73L)
// class WDiEdge[N](nodes: Product,
//                  weight: Long)
//   extends WUnDiEdge [N](nodes, weight)
//   with    DiEdgeLike[N]
//   with    EdgeCopy  [WDiEdge]
//   with    OuterEdge    [N,WDiEdge]
// {
//   override protected[collection]
//   def copy[NN](newNodes: Product) = new WDiEdge[NN](newNodes, weight)
// }
// object WDiEdge extends WEdgeCompanion[WDiEdge] {
//   override def newEdge[N](nodes: Product, weight: Long) = new WDiEdge[N](nodes, weight)
// }
// // ------------------------------------------------------------------------ Wk*
// import WkBase._
// /** key-weighted undirected edge. */
// abstract class WkUnDiEdge[N](nodes: Product, weight: Long)
//   extends WUnDiEdge[N](nodes, weight)
//   with    OuterEdge   [N,WkUnDiEdge]
//   with    WkEdge   [N]
//   with    EqUnDi
// object WkUnDiEdge extends WkEdgeCompanion[WkUnDiEdge] {
//   @SerialVersionUID(975L) override
//   def newEdge[N](nodes: Product, weight: Long) =
//     new  WkUnDiEdge[N](nodes, weight)
//     with EdgeCopy [WkUnDiEdge] { 
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
//     }
// }
// /** key-weighted directed edge. */
// abstract class WkDiEdge[N](nodes: Product, weight: Long)
//   extends WkUnDiEdge[N](nodes, weight)
//   with    DiEdgeLike[N]
//   with    OuterEdge    [N,WkDiEdge]
//   with    EqDi
// object WkDiEdge extends WkEdgeCompanion[WkDiEdge] {
//   @SerialVersionUID(976L) override
//   def newEdge[N](nodes: Product, weight: Long) =
//     new  WkDiEdge[N](nodes, weight)
//     with EdgeCopy [WkDiEdge] { 
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
//     }
// }
// ------------------------------------------------------------------------- L*
/** labeled undirected edge. */
abstract class LUnDiEdge[N](n1: N, n2: N)
  extends UnDiEdge[N](n1, n2)
  with    OuterEdge[N,LUnDiEdge]
  with    LEdge[N]
  with    EdgeCopy[LUnDiEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LUnDiEdge.newEdge[NN,L1](newNodes, label)
}
object LUnDiEdge extends LEdgeCompanion[LUnDiEdge] {
  override def apply[N,L](n1: N, n2: N)(l: L) =
    new LUnDiEdge[N](n1, n2) {
      type L1 = L
      override val label = l
    }
}
/** labeled directed edge. */
abstract class LDiEdge[N](override val source: N, override val target: N)
  extends LUnDiEdge[N](source, target)
  with    DiEdgeLike[N]
  with    OuterEdge[N,LDiEdge]
  with    EdgeCopy[LDiEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LDiEdge.newEdge[NN,L1](newNodes, label)
}
object LDiEdge extends LEdgeCompanion[LDiEdge] {
  override def apply[N,L](source: N, target: N)(l: L) =
    new LDiEdge[N](source, target) {
      type L1 = L
      override val label = l
    }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._
/** key-labeled undirected edge. */
abstract class LkUnDiEdge[N](n1: N, n2: N)
  extends LUnDiEdge[N](n1, n2)
  with    OuterEdge[N,LkUnDiEdge]
  with    LkEdge[N]
  with    EqUnDi
  with    EdgeCopy[LkUnDiEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LkUnDiEdge.newEdge[NN,L1](newNodes, label)
}
object LkUnDiEdge extends LkEdgeCompanion[LkUnDiEdge] {
  override def apply[N,L](n1: N, n2: N)(l: L) =
    new LkUnDiEdge[N](n1, n2) {
      type L1 = L
      override val label = l
    }
}
/** key-labeled directed edge. */
abstract class LkDiEdge[N](source: N, target: N)
  extends LDiEdge[N](source, target)
  with    OuterEdge[N,LkDiEdge]
  with    LkEdge[N]
  with    EqDi
  with    EdgeCopy[LkDiEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LkDiEdge.newEdge[NN,L1](newNodes, label)
}
object LkDiEdge extends LkEdgeCompanion[LkDiEdge] {
  override def apply[N,L](source: N, target: N)(l: L) =
    new LkDiEdge[N](source, target) {
      type L1 = L
      override val label = l
    }
}
// // ------------------------------------------------------------------------ WL*
// import WLBase._
// /** weighted, labeled undirected edge. */
// abstract class WLUnDiEdge[N](nodes: Product, weight: Long)
//   extends WUnDiEdge[N](nodes, weight)
//   with    OuterEdge  [N,WLUnDiEdge]
//   with    LEdge   [N]
//   with    WLEdge  [N]
// object WLUnDiEdge extends WLEdgeCompanion[WLUnDiEdge] {
//   @SerialVersionUID(981L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLUnDiEdge[N](nodes, weight)
//     with EdgeCopy [WLUnDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** weighted, labeled directed edge. */
// abstract class WLDiEdge[N](nodes: Product, weight: Long)
//   extends WLUnDiEdge[N](nodes, weight) 
//   with    DiEdgeLike[N]
//   with    OuterEdge    [N,WLDiEdge]
// object WLDiEdge extends WLEdgeCompanion[WLDiEdge] {
//   @SerialVersionUID(982L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLDiEdge [N](nodes, weight)
//     with EdgeCopy[WLDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ----------------------------------------------------------------------- WkL*
// import WkLBase._
// /** key-weighted, labeled undirected edge. */
// abstract class WkLUnDiEdge[N](nodes: Product, weight: Long)
//   extends WLUnDiEdge[N](nodes, weight)
//   with    WkEdge    [N]
//   with    OuterEdge    [N,WkLUnDiEdge]
//   with    EqUnDi
// object WkLUnDiEdge extends WkLEdgeCompanion[WkLUnDiEdge] {
//   @SerialVersionUID(983L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLUnDiEdge[N](nodes, weight)
//     with EdgeCopy [WkLUnDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** key-weighted, labeled directed edge. */
// abstract class WkLDiEdge[N](nodes: Product, weight: Long)
//   extends WkLUnDiEdge[N](nodes, weight) 
//   with    DiEdgeLike [N]
//   with    OuterEdge     [N,WkLDiEdge]
//   with    EqDi
// object WkLDiEdge extends WkLEdgeCompanion[WkLDiEdge] {
//   @SerialVersionUID(984L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLDiEdge [N](nodes, weight)
//     with EdgeCopy[WkLDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ----------------------------------------------------------------------- WLk*
// import WLkBase._
// /** weighted, key-labeled undirected edge. */
// abstract class WLkUnDiEdge[N](nodes: Product, weight: Long)
//   extends WLUnDiEdge[N](nodes, weight) 
//   with    OuterEdge   [N,WLkUnDiEdge]
//   with    LkEdge   [N]
//   with    WLkEdge  [N]
//   with    EqUnDi
// object WLkUnDiEdge extends WLkEdgeCompanion[WLkUnDiEdge] {
//   @SerialVersionUID(985L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLkUnDiEdge[N](nodes, weight)
//     with EdgeCopy  [WLkUnDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** weighted, key-labeled directed edge. */
// abstract class WLkDiEdge[N](nodes: Product, weight: Long)
//   extends WLkUnDiEdge[N](nodes, weight) 
//   with    DiEdgeLike [N]
//   with    OuterEdge     [N,WLkDiEdge]
//   with    EqDi
// object WLkDiEdge extends WLkEdgeCompanion[WLkDiEdge] {
//   @SerialVersionUID(986L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLkDiEdge[N](nodes, weight)
//     with EdgeCopy[WLkDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ---------------------------------------------------------------------- WkLk*
// import WkLkBase._
// /** key-weighted, key-labeled undirected edge. */
// abstract class WkLkUnDiEdge[N](nodes: Product, weight: Long)
//   extends WLUnDiEdge[N](nodes, weight) 
//   with    OuterEdge    [N,WkLkUnDiEdge]
//   with    WkLkEdge  [N]
//   with    EqUnDi
// object WkLkUnDiEdge extends WkLkEdgeCompanion[WkLkUnDiEdge] {
//   @SerialVersionUID(987L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLkUnDiEdge[N](nodes, weight)
//     with EdgeCopy  [WkLkUnDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** key-weighted, key-labeled directed edge. */
// abstract class WkLkDiEdge[N](nodes: Product, weight: Long)
//   extends WkLkUnDiEdge[N](nodes, weight) 
//   with    DiEdgeLike  [N]
//   with    OuterEdge      [N,WkLkDiEdge]
//   with    EqDi
// object WkLkDiEdge extends WkLkEdgeCompanion[WkLkDiEdge] {
//   @SerialVersionUID(988L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLkDiEdge[N](nodes, weight)
//     with EdgeCopy[WkLkDiEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
