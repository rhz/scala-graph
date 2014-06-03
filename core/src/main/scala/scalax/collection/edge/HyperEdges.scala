package scalax.collection.edge

import scalax.collection.GraphEdge._,
       scalax.collection.GraphPredef._
import scalax.collection.Graph
// import WBase._, LBase._
import LBase._

// // ------------------------------------------------------------------------- W*
// /** weighted, undirected hyperedge. */
// @SerialVersionUID(70L)
// class WHyperEdge[N](nodes: Product,
//                     override val weight: Long)
//   extends HyperEdge[N](nodes)
//   with    WEdge    [N]
//   with    EdgeCopy [WHyperEdge]
//   with    OuterEdge   [N,WHyperEdge]
// {
//   override protected[collection]
//   def copy[NN](newNodes: Product) = new WHyperEdge[NN](newNodes, weight)
// }
// object WHyperEdge extends WHyperEdgeCompanion[WHyperEdge] {
//   def newEdge[N](nodes: Product, weight: Long) = new WHyperEdge[N](nodes, weight)
// }
// /** weighted directed hyperedge. */
// @SerialVersionUID(71L)
// class WDiHyperEdge[N](nodes: Product,
//                       override val weight: Long)
//   extends DiHyperEdge[N](nodes)
//   with    WEdge     [N]
//   with    EdgeCopy  [WDiHyperEdge]
//   with    OuterEdge    [N,WDiHyperEdge]
// {
//   override protected[collection]
//   def copy[NN](newNodes: Product) = new WDiHyperEdge[NN](newNodes, weight)
// }
// object WDiHyperEdge extends WHyperEdgeCompanion[WDiHyperEdge] {
//   def newEdge[N](nodes: Product, weight: Long) = new WDiHyperEdge[N](nodes, weight)
// }
// // ------------------------------------------------------------------------ Wk*
// import WkBase._
// /** key-weighted undirected hyperedge. */
// abstract class WkHyperEdge[N](nodes: Product, weight: Long)
//   extends WHyperEdge[N](nodes, weight)
//   with    OuterEdge  [N,WkHyperEdge]
//   with    WkEdge  [N]
// object WkHyperEdge extends WkHyperEdgeCompanion[WkHyperEdge] {
//   @SerialVersionUID(875L) override
//   def newEdge[N](nodes: Product, weight: Long) =
//     new  WkHyperEdge[N](nodes, weight)
//     with EdgeCopy [WkHyperEdge] { 
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
//     }
// }
// /** key-weighted directed hyperedge. */
// abstract class WkDiHyperEdge[N](nodes: Product, weight: Long)
//   extends WkHyperEdge    [N](nodes, weight)
//   with    DiHyperEdgeLike[N]
//   with    OuterEdge         [N,WkDiHyperEdge]
// object WkDiHyperEdge extends WkHyperEdgeCompanion[WkDiHyperEdge] {
//   @SerialVersionUID(876L) override
//   def newEdge[N](nodes: Product, weight: Long) =
//     new  WkDiHyperEdge[N](nodes, weight)
//     with EdgeCopy [WkDiHyperEdge] { 
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN](newNodes, weight)
//     }
// }
// ------------------------------------------------------------------------- L*
/** labeled undirected hyperedge. */
abstract class LHyperEdge[N](nodes: Iterable[N])
  extends HyperEdge[N](nodes)
  with    OuterEdge[N,LHyperEdge]
  with    LEdge[N]
  with    EdgeCopy[LHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LHyperEdge.newEdge[NN,L1](newNodes, label)
}
object LHyperEdge extends LHyperEdgeCompanion[LHyperEdge] {
  @SerialVersionUID(871L) override
  def newEdge[N,L](nodes: Iterable[N], l: L) =
    new LHyperEdge[N](nodes)
    with EdgeCopy[LHyperEdge] {
      type L1 = L
      override val label = l
    }
}
/** Labeled directed hyperedge. */
abstract class LDiHyperEdge[N](override val sources: Iterable[N], override val targets: Iterable[N])
  extends LHyperEdge[N](sources ++ targets)
  with    DiHyperEdgeLike[N]
  with    OuterEdge[N,LDiHyperEdge]
  with    EdgeCopy[LDiHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LDiHyperEdge[NN,L1](newNodes take sources.size, newNodes drop sources.size)(label)
}
object LDiHyperEdge extends EdgeCompanionBase[LDiHyperEdge] {
  def apply[N,L](sources: Iterable[N], targets: Iterable[N])(l: L) =
    new LDiHyperEdge[N](sources, targets) {
      type L1 = L
      override val label = l
    }
}
// ------------------------------------------------------------------------ Lk*
import LkBase._
/** key-labeled undirected hyperedge. */
abstract class LkHyperEdge[N](nodes: Iterable[N])
  extends LHyperEdge[N](nodes)
  with    OuterEdge[N,LkHyperEdge]
  with    LkEdge[N]
  with    EdgeCopy[LkHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LkHyperEdge.newEdge[NN,L1](newNodes, label)
}
object LkHyperEdge extends LkHyperEdgeCompanion[LkHyperEdge] {
  @SerialVersionUID(872L) override
  def newEdge[N,L](nodes: Iterable[N], l: L) =
    new LkHyperEdge[N](nodes) {
      type L1 = L
      override val label = l
    }
}
/** key-labeled directed hyperedge. */
abstract class LkDiHyperEdge[N](sources: Iterable[N], targets: Iterable[N])
  extends LDiHyperEdge[N](sources, targets)
  with    OuterEdge[N,LkDiHyperEdge]
  with    LkEdge[N]
  with    EdgeCopy[LkDiHyperEdge] {
  override protected[collection] def copy[NN](newNodes: Iterable[NN]) =
    LkDiHyperEdge[NN,L1](newNodes take sources.size, newNodes drop sources.size)(label)
}
object LkDiHyperEdge extends EdgeCompanionBase[LkDiHyperEdge] {
  def apply[N,L](sources: Iterable[N], targets: Iterable[N])(l: L) =
    new LkDiHyperEdge[N](sources, targets) {
      override val label = l
      type L1 = L
    }
}
// // ------------------------------------------------------------------------ WL*
// import WLBase._
// /** weighted, labeled undirected hyperedge. */
// abstract class WLHyperEdge[N](nodes: Product, weight: Long)
//   extends WHyperEdge[N](nodes, weight)
//   with    OuterEdge  [N,WLHyperEdge]
//   with    LEdge   [N]
//   with    WLEdge  [N]
// object WLHyperEdge extends WLHyperEdgeCompanion[WLHyperEdge] {
//   @SerialVersionUID(881L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLHyperEdge[N](nodes, weight)
//     with EdgeCopy [WLHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** weighted, labeled directed hyperedge. */
// abstract class WLDiHyperEdge[N](nodes: Product, weight: Long)
//   extends WLHyperEdge    [N](nodes, weight) 
//   with    DiHyperEdgeLike[N]
//   with    OuterEdge         [N,WLDiHyperEdge]
// object WLDiHyperEdge extends WLHyperEdgeCompanion[WLDiHyperEdge] {
//   @SerialVersionUID(882L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLDiHyperEdge [N](nodes, weight)
//     with EdgeCopy[WLDiHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ----------------------------------------------------------------------- WkL*
// import WkLBase._
// /** key-weighted, labeled undirected hyperedge. */
// abstract class WkLHyperEdge[N](nodes: Product, weight: Long)
//   extends WLHyperEdge[N](nodes, weight)
//   with    WkEdge    [N]
//   with    OuterEdge    [N,WkLHyperEdge]
// object WkLHyperEdge extends WkLHyperEdgeCompanion[WkLHyperEdge] {
//   @SerialVersionUID(883L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLHyperEdge[N](nodes, weight)
//     with EdgeCopy [WkLHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** key-weighted, labeled directed hyperedge. */
// abstract class WkLDiHyperEdge[N](nodes: Product, weight: Long)
//   extends WkLHyperEdge   [N](nodes, weight) 
//   with    DiHyperEdgeLike[N]
//   with    OuterEdge         [N,WkLDiHyperEdge]
// object WkLDiHyperEdge extends WkLHyperEdgeCompanion[WkLDiHyperEdge] {
//   @SerialVersionUID(884L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLDiHyperEdge [N](nodes, weight)
//     with EdgeCopy[WkLDiHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ----------------------------------------------------------------------- WLk*
// import WLkBase._
// /** weighted, key-labeled undirected hyperedge. */
// abstract class WLkHyperEdge[N](nodes: Product, weight: Long)
//   extends WLHyperEdge[N](nodes, weight) 
//   with    OuterEdge   [N,WLkHyperEdge]
//   with    LkEdge   [N]
//   with    WLkEdge  [N]
// object WLkHyperEdge extends WLkHyperEdgeCompanion[WLkHyperEdge] {
//   @SerialVersionUID(885L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLkHyperEdge[N](nodes, weight)
//     with EdgeCopy  [WLkHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** weighted, key-labeled directed hyperedge. */
// abstract class WLkDiHyperEdge[N](nodes: Product, weight: Long)
//   extends WLkHyperEdge   [N](nodes, weight) 
//   with    DiHyperEdgeLike[N]
//   with    OuterEdge         [N,WLkDiHyperEdge]
// object WLkDiHyperEdge extends WLkHyperEdgeCompanion[WLkDiHyperEdge] {
//   @SerialVersionUID(886L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WLkDiHyperEdge[N](nodes, weight)
//     with EdgeCopy[WLkDiHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// // ---------------------------------------------------------------------- WkLk*
// import WkLkBase._
// /** key-weighted, key-labeled undirected hyperedge. */
// abstract class WkLkHyperEdge[N](nodes: Product, weight: Long)
//   extends WLHyperEdge[N](nodes, weight) 
//   with    OuterEdge    [N,WkLkHyperEdge]
//   with    WkLkEdge  [N]
// object WkLkHyperEdge extends WkLkHyperEdgeCompanion[WkLkHyperEdge] {
//   @SerialVersionUID(887L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLkHyperEdge[N](nodes, weight)
//     with EdgeCopy  [WkLkHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
// /** key-weighted, key-labeled directed hyperedge. */
// abstract class WkLkDiHyperEdge[N](nodes: Product, weight: Long)
//   extends WkLkHyperEdge  [N](nodes, weight) 
//   with    DiHyperEdgeLike[N]
//   with    OuterEdge         [N,WkLkDiHyperEdge]
// object WkLkDiHyperEdge extends WkLkHyperEdgeCompanion[WkLkDiHyperEdge] {
//   @SerialVersionUID(888L) override
//   def newEdge[N,L](nodes: Product, weight: Long, pLabel: L) =
//     new  WkLkDiHyperEdge[N](nodes, weight)
//     with EdgeCopy[WkLkDiHyperEdge] { 
//       type L1 = L
//       override val label = pLabel
//       override protected[collection]
//       def copy[NN](newNodes: Product) = newEdge[NN,L](newNodes, weight, pLabel)
//     }
// }
