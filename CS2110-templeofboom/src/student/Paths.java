package student;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import game.Edge;
import game.EscapeState;
import game.Node;

/** This class contains Dijkstra's shortest-path algorithm and some other methods. */
public class Paths {

    /** Return a shortest path from start to end, or the empty list
     *  if one does not exist.
     *  
     * The result contains a list of Nodes on the shortest path,
     * including start and end, or the empty list if no path exists. */
    public static <T> List<Node> dijkstra(Node start, Node end) {
        // TODO Implement Disjktras's shortest-path algorithm as presented
        // in the last few slides of the notes for lecture 19. In particular,
        // a min-heap (as implemented in assignment A6) should be used for
        // the frontier set.

        // The frontier set, as discussed in lecture
        Heap<Node> frontier= new Heap<Node>();

        // Each Node in the Settled and Frontier sets has an entry
        // that gives the distance and the backpointer of the Node.
        HashMap<Node, NodeInfo> NodeInfo= new HashMap<Node, NodeInfo>();

        frontier.add(start, 0);
        NodeInfo.put(start, new NodeInfo());
        // invariant: As presented in notes for Lecture 19
        while (frontier.size() > 0) {
            Node f= frontier.poll();
            if (f == end) {
                return buildPath(end, NodeInfo);
            }
            
            NodeInfo fInfo= NodeInfo.get(f);

            Set<Edge> edges= f.getExits();
            for (Edge edge : edges) {
                Node w= edge.getOther(f);
                NodeInfo wInfo= NodeInfo.get(w);
                int wDistance= fInfo.distance + edge.length;
                if (wInfo == null) {
                    frontier.add(w, wDistance);
                    NodeInfo.put(w, new NodeInfo(f, wDistance));
                } else 
                    if (wDistance < wInfo.distance) {
                        frontier.changePriority(w, wDistance);
                        wInfo.distance= wDistance;
                        wInfo.backPointer= f;
                    }
            }

        }

        return new ArrayList<Node>(); //no path was found

    }

    /** Return the path from the start Node to end.
     * Precondition: NodeInfo contains all the necessary information about
     * the path. */
    private static <T> List<Node> buildPath(Node end, HashMap<Node, NodeInfo> NodeInfo) {
        List<Node> path= new ArrayList<Node>();
        Node p= end;
        while (p != null) {
            path.add(p);
            p= NodeInfo.get(p).backPointer;
        }
        Collections.reverse(path);
        return path;
    }

    /** Return the sum of the weight of edges on path p. */
    public static <T> int pathLength(List<Node> path) {
        synchronized(path) {
            if (path.size() == 0) return 0;

            Iterator<Node> iter= path.iterator();
            Node p= iter.next();  // First Node on path
            int s= 0;
            // invariant: s is the sum of edges from start up to Node p
            while (iter.hasNext()) {
                Node q= iter.next();
                s= s + p.getEdge(q).length;
                p= q;
            }
            return s;
        }
    }

    /** An instance contains information about a Node: the previous
     * Node on a shortest path from the start Node to this Node and the distance
     * of this Node from the start Node. */
    private static class NodeInfo {
        private Node backPointer;
        private int distance;

        /** Constructor: an instance with distance d from the start Node and
         * backpointer p.*/
        private NodeInfo(Node p, int d) {
            backPointer= p;  // Backpointer on the path (null if start Node)
            distance= d; //Distance from start Node to this one.
        }

        /** Constructor: an instance with a null previous Node and distance 0. */
        private NodeInfo() {}

        /** return a represebtation of this instance. */
        public String toString() {
            return "distance " + distance + ", bckptr " + backPointer;
        }
    }

    /** Return a HashSet with all the tiles with gold corresponding to its amount. */
    public static <T> HashSet<Node> allGolds(EscapeState state) {
    	Collection<Node> allMoves = state.getNodes();
		HashSet<Node> goldTiles = new HashSet<Node>();
		for(Node moves : allMoves){
			int gold = moves.getTile().getGold();
			if (gold!=0) goldTiles.add(moves);
		}
		return goldTiles;
    }


    /** Return the total among of gold along the path. 
     * Precondition: path is not empty nor null.*/
    public static <T> int pathGoldAmount(List<Node> path){
    	int amount = 0;
    	for (Node tile: path){
    		amount = amount + tile.getTile().getGold();
    	}
    	return amount;
    }
}
