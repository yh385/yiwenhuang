package game;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Set;

public class Node {
 
    /** The unique numerical identifier of this Node */
    private final long id;
    /** Represents the edges outgoing from this Node */
    private final Set<Edge> edges;
    private final Set<Node> neighbors;

    private final Set<Edge> unmodifiableEdges;
    private final Set<Node> unmodifiableNeighbors;
    
    /** Extra state that belongs to this node */
    private final Tile tile;
    
    /* package */ Node(Tile t, int numCols) {
     this(t.getRow() * numCols + t.getColumn(), t);
    }

    /* package */ Node(long givenId, Tile t) {
        id= givenId;
        edges= new LinkedHashSet<>();
        neighbors= new LinkedHashSet<>();

        unmodifiableEdges= Collections.unmodifiableSet(edges);
        unmodifiableNeighbors= Collections.unmodifiableSet(neighbors);

        tile= t;
    }
    
    /* package */ void addEdge(Edge e) {
        edges.add(e);
        neighbors.add(e.getOther(this));
    }
    
    /** 
     * Returns the unique Identifier of this Node.
     * 
     * @return The unique Identifier of this Node.  
     */
    public long getId() {
        return id;
    }
    
    /**
     * Returns the Edge of this Node that connects to Node q.
     * 
     * @param q A Node that neighbors this Node
     * @return The Edge of this Node that connects to Node q.
     */
    public Edge getEdge(Node q) {
        for (Edge e : edges) {
            if (e.getDest().equals(q)) {
                return e;
            }
        }
        throw new IllegalArgumentException("getEdge: Node must be a neighbor of this Node");
    }
    
    /** 
     * Returns an unmodifiable view of the Edges leaving this Node. 
     * 
     * @return An unmodifiable view of the Edges leaving this Node.
     */
    public Set<Edge> getExits() {
     return unmodifiableEdges;
    }
    
    /** 
     * Returns an unmodifiable view of the Nodes neighboring this Node.
     * 
     * @return An unmodifiable view of the Nodes neighboring this Node.
     */
    public Set<Node> getNeighbors() {
        return unmodifiableNeighbors;
    }
    
    /** 
     * Returns the Tile corresponding to this Node.
     * 
     * @return The Tile corresponding to this Node. 
     */
    public Tile getTile() {
        return tile;
    }
    
    @Override public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof Node)) {
            return false;
        }
        return id == ((Node)o).id;
    }
    
    @Override public int hashCode() {
        return Objects.hash(id);
    }
}
