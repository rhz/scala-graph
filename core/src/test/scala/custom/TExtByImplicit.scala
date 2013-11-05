package custom

import org.scalatest.Suite
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

@RunWith(classOf[JUnitRunner])
class TExtByImplicitTest
	extends	Suite
	with	  ShouldMatchers
{
  def test_graphEnrichment {
    /* Provide graph enrichment.
     */
    final class ExtGraph[N, E[X] <: EdgeLikeIn[X]](val g: Graph[N,E]) {
      
      /* Set of all directed edges contained in g.
       */
      def diEdges = g.edges filter (_.directed)
    }
    implicit def gToExtG[N, E[X] <: EdgeLikeIn[X]](g: Graph[N,E]) =
      new ExtGraph[N,E](g)

    /* Consume graph enrichment.
     */
    Graph(1~2,  2~3).diEdges should be ('isEmpty)
    Graph(1~2, 2~>3).diEdges should have size (1)
  }
  
  def test_unDiGraphEnrichment {
    /* Provide graph enrichment restricted to undirected graphs.
     * Note that a lower bound must also be defined to exclude directed edges.
     */
    final class UnDiGraph[N, E[X] >: UnDiEdge[X] <: UnDiEdge[X]]
        (val g: Graph[N,E]) {
      def alwaysTrue = true
    }
    implicit def gToUnDiG[N, E[X] >: UnDiEdge[X] <: UnDiEdge[X]](g: Graph[N,E]) =
      new UnDiGraph[N,E](g)
    
    /* Consume enrichment.  
     */
    Graph(1~2).alwaysTrue should be (true)
    /* Comment in the following lines to assure of getting compiler errors
     * when using other then undirected graphs.
     */
    // Graph(1~2~3).alwaysTrue 
    // Graph(1~>2).alwaysTrue
  }
  
  def test_nodeEnrichment_1 {
    /* Provide node enrichment.
     * Note that this way of enrichment is not suitable for methods returning
     * inner nodes or inner edges.
     */
    final class ExtGraphNode[N, E[X] <: EdgeLikeIn[X]]
        (node_ : Graph[N,E]#NodeT) {
      type NodeT = graph.NodeT
      val graph = node_.containingGraph
      val node  = node_.asInstanceOf[NodeT]

      /* Calculates the sum of weights of outgoing edges from this node.
       */
      def outgoingWeights = node.outgoing.map(_.weight).sum
    }
    implicit def nodeToExtN[N, E[X] <: EdgeLikeIn[X]] (node: Graph[N,E]#NodeT) =
      new ExtGraphNode[N,E](node)
    
    /* Consume node enrichment.
     */
    import scalax.collection.edge.Implicits._

    val g = Graph((1~%2)(1),  (1~%3)(2), (2~%3)(3))
    (g get 1).outgoingWeights should be (3)
    (g get 2).outgoingWeights should be (4)
    (g get 3).outgoingWeights should be (5)
  }
}