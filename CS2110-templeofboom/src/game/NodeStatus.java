package game;

import java.util.Objects;

public class NodeStatus implements Comparable<NodeStatus> {
    private final long id;
    private final int distance;

    /* package */ NodeStatus(long nodeId, int dist) {
        id= nodeId;
        distance= dist;
    }

    /**
     * Returns the Id of the Node that corresponds to this NodeStatus.
     * 
     * @return The Id of the Node that corresponds to this NodeStatus.
     */
    public long getId() {
        return id;
    }

    /**
     * Returns the distance to the orb from the Node that corresponds to
     * this NodeStatus.
     * 
     * @return The distance to the orb from the Node that corresponds to
     * this NodeStatus.
     */
    public int getDistanceToTarget() {
        return distance;
    }

    @Override
    public int compareTo(NodeStatus other) {
        return Integer.compare(distance, other.distance);
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof NodeStatus)) {
            return false;
        }
        return id == ((NodeStatus)o).id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}
