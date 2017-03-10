package game;

import java.util.Map;

/**
 * An Edge represents an immutable directed, weighted edge.
 * @author eperdew
 */
public class Edge {

    /** The Node this edge is coming from */
    private final Node src;
    /** The node this edge is going to */
    private final Node dest;
    /** The length of this edge */
    public final int length;


    public Edge(Node src, Node dest, int length) {
        this.src= src;
        this.dest= dest;
        this.length= length;
    }

    public Edge(Edge e, Map<Node,Node> isomorphism) {
        src = isomorphism.get(e.src);
        dest = isomorphism.get(e.dest);
        length = e.length;
    }

    /**
     * Returns the <tt>Node</tt> on this <tt>Edge</tt> that is not equal to the one provided.
     * Throws an <tt>IllegalArgumentException</tt> if <tt>n</tt> is not in this <tt>Edge</tt>.
     * @param n A <tt>Node</tt> on this <tt>Edge</tt>
     * @return The <tt>Node</tt> not equal to <tt>n</tt> on this <tt>Edge</tt>
     */
    public Node getOther(Node n) {
        if (src == n) {
            return dest;
        }
        else if (dest == n) {
            return src;
        }
        else {
            throw new IllegalArgumentException("getOther: Edge must contain provided node");
        }
    }

    /**
     * Returns the length of this <tt>Edge</tt>
     * @return the length of this <tt>Edge</tt>
     */
    public int length() {
        return length;
    }

    /**
     * Returns the <tt>Node</tt> that this edge is coming from.
     * @return The <tt>Node</tt> that this edge is coming from.
     */
    public Node getSource() {
        return src;
    }

    /**
     * Returns the <tt>Node</tt> that this edge is going to.
     * @return The <tt>Node</tt> that this edge is going to.
     */
    public Node getDest() {
        return dest;
    }
}
