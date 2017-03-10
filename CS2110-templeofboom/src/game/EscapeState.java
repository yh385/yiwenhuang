package game;

import java.util.Collection;

/**
 * The state of the game while escaping from the cavern.
 * An EscapeState provides all the information necessary to
 * get out of the cavern and collect gold on the way.
 * 
 * This interface provides access to the complete graph of the cavern,
 * which will allow computation of the path.
 * Once you have determined how Tennessee should get out, call
 * moveTo(Node) repeatedly to move to each node and pickUpGold() to collect
 * gold on the way out. */
public interface EscapeState {
    /** Return the Node corresponding to Tennessee's location in the graph. */
    public Node currentNode();

    /** Return the Node associated with the exit from the cavern.
     * Tennessee has to move to this Node in order to get out. */
    public Node getExit();

    /**Return a collection containing all the nodes in the graph.
     * They in no particular order. */
    public Collection<Node> getNodes();

    /** Change Tennessee's location to n.
     * Throw an IllegalArgumentException if n is not directly connected to
     * Tennessee's  location. */
    public void moveTo(Node n);

    /** Pick up the gold on the current tile.
     * Throw an IllegalStateException if there is no gold at the current location,
     *     either because there never was any or because it was already picked up. */
    public void seizeGold();

    /** Return the steps remaining to get out of the cavern.
     * This value will change with every call to moveTo(Node),
     * and if it reaches 0 before you get out, you have failed to get out.  */
    public int stepsRemaining();
}
