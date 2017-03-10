package game;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Random;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class represents a Cavern that the explorer can navigate through.
 * The cavern is set up as a grid of Tile objects with a weighted graph of all
 * non-floor tiles.
 *
 * There is also an entrance to the cavern and a target location (which may also be the entrance).
 */
public class Cavern {

    public static final int MAX_EDGE_WEIGHT= 15;
    private static final double DENSITY= 0.6;
    private static final double GOLD_PROBABILITY= 0.33;
    public static final int MAX_GOLD_VALUE= 1000;
    public static final int TASTY_VALUE= 5000;

    /**
      * An enum reprsenting a grid direction.
      */
    public enum Direction {
        NORTH(-1,0), EAST(0,1), SOUTH(1,0), WEST(0,-1);

        private final Point dir;

        private Direction(int row, int col) {
            dir= new Point(row, col);
        }

        public Point getPoint() {
            return dir;
        }
    }

    /**
     * An enum representing a point on the grid.
     */
    private static class Point {
        private final int row;
        private final int col;

        private Point(int r, int c) {
            row= r;
            col= c;
        }

        public Point add(Point p) {
            return new Point(row + p.row, col + p.col);
        }

        @Override public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Point)) return false;
            Point p= (Point) o;
            return p.row == row && p.col == col;
        }

        @Override public int hashCode() {
            return Objects.hash(row, col);
        }
    }

    private final int rows;
    private final int cols;

    private final Set<Node> graph;
    private final Node entrance;
    private final Node target;

    private final Node[][] tiles;

    /**
     * Creates a random cavern of the given size where there is no gold, all edges have weight 1,
     * and there is an orb a reasonable distance from the exit.
     *
     * @param rows The number of rows in this cavern.
     * @param cols The number of columns in this cavern.
     * @param rand A source of randomness to use for the cavern generation.
     */
    public static Cavern digExploreCavern(int rows, int cols, Random rand) {
        int minOrbDist= minOrbDistance(rows, cols);

        Cavern cavern= new Cavern(rows, cols, rand, () -> 1, () -> 0, Tile.Type.ORB);
        while (cavern.minPathLengthToTarget(cavern.getEntrance()) < minOrbDist) {
            cavern= new Cavern(rows, cols, rand, () -> 1, () -> 0, Tile.Type.ORB);
        }
        return cavern;
    }

    /**
     * Returns the minimum allowable path distance from the entrance to the orb.
     */
    private static int minOrbDistance(int rows, int cols) {
        return (rows + cols) / 2;
    }

    /**
     * Generates a new random cavern with random gold and edge weights.
     * It is guaranteed that ({@code currentRow}, {@code currentCol}) will be an open floor cell.
     *
     * @param rows The number of rows in this cavern.
     * @param cols The number of columns in this cavern.
     * @param currentRow The row of the cell that must be open floor.
     * @param currentCol The column of the cell that must be open floor.
     * @param rand A source of randomness to use for the cavern generation.
     */
    public static Cavern digEscapeCavern(int rows, int cols, int currentRow, int currentCol, Random rand) {
        Supplier<Integer> edgeWeightGen= () -> rand.nextInt(MAX_EDGE_WEIGHT) + 1;
        Supplier<Integer> goldGen= () -> Cavern.generateGoldValue(rand);
        Cavern potentialCavern= new Cavern(rows, cols, rand, edgeWeightGen, goldGen, Tile.Type.ENTRANCE);
        while (potentialCavern.getTileAt(currentRow, currentCol).getType() != Tile.Type.FLOOR) {
            potentialCavern= new Cavern(rows, cols, rand, edgeWeightGen, goldGen, Tile.Type.ENTRANCE);
        }
        return potentialCavern;
    }

    /**
     * Returns a randomly determined gold value for a given tile.
     */
    private static int generateGoldValue(Random rand) {
        if (rand.nextDouble() > GOLD_PROBABILITY) {
            return 0;
        }

        int val= rand.nextInt(MAX_GOLD_VALUE) + 1;
        if (val == MAX_GOLD_VALUE) {
            val= TASTY_VALUE;
        }
        return val;
    }

    /**
     * Constructor: Creates a new cavern of the specified size.
     * The given randomness is used to determine which grid tiles are open.
     * This uses {@code edgeWeightGenerator} and {@code goldGenerator} to generate edge weights and gold values.
     * Precondition: {@code targetType} must be either {@code Tile.Type.ORB} or {@code Tile.Type.ENTRANCE}.
     */
    private Cavern(int rws,
                   int cls,
                   Random rand,
                   Supplier<Integer> edgeWeightGenerator,
                   Supplier<Integer> goldGenerator,
                   Tile.Type targetType) {
        rows= rws;
        cols= cls;
        
        graph= generateGraph(rand, targetType, goldGenerator);
        entrance= graph.stream().filter((n) -> n.getTile().getType() == Tile.Type.ENTRANCE).findAny().get();
        target= graph.stream().filter((n) -> n.getTile().getType() == targetType).findAny().get();

        // Set tiles for the floor and then add walls wherever floor is missing.
        tiles= new Node[rows][cols];
        for (Node node : graph) {
            Tile t= node.getTile();
            tiles[t.getRow()][t.getColumn()]= node;
        }
        for (int i= 0; i < rows; i++) {
            for (int j= 0; j < cols; j++) {
                if (tiles[i][j] == null) {
                    tiles[i][j]= new Node(new Tile(i, j, 0, Tile.Type.WALL), cols);
                }
            }
        }
        createEdges(tiles, edgeWeightGenerator);
    }

    /**
     * Constructor constructs a graph from the given graph and tiles with the given target.
     * Preconditions:
     *    1. {@code givenGraph} and {@code givenTiles} represent the same graph
     *       (i.e. {@code givenGraph} contains all non-floor nodes in {@code givenTiles} and edges are along the grid).
     *    2. {@code trgt} is a node in {@code givenGraph}.
     */
    private Cavern(Set<Node> givenGraph, Node[][] givenTiles, Node trgt) {
        tiles= givenTiles;
        rows= tiles.length;
        cols= tiles[0].length;

        graph= Collections.unmodifiableSet(givenGraph);
        entrance= graph.stream().filter((n) -> n.getTile().getType() == Tile.Type.ENTRANCE).findAny().get();
        target= trgt;
    }

    /**
     * Given a grid of nodes without edges and a supplier to return edge weights,
     * adds edges to the nodes between adjacent non-wall tiles.
     * Precondition: all elements of {@code tiles} are non-null.
     */
    private void createEdges(Node[][] tiles, Supplier<Integer> edgeWeightGenerator) {
        for (int i= 0; i < tiles.length - 1; i++) {
            for (int j= 0; j < tiles[i].length - 1; j++) {
                Node node= tiles[i][j];
                if (node.getTile().getType() == Tile.Type.WALL) {
                    continue;
                }

                final Point p= new Point(i, j);
                Stream.of(Direction.SOUTH, Direction.EAST)
                      .map(Direction::getPoint)
                      .map(p::add)
                      .map((q) -> tiles[q.row][q.col])
                      .filter((m) -> m.getTile().getType() != Tile.Type.WALL)
                      .forEach((m) -> {
                          int weight= edgeWeightGenerator.get();
                          node.addEdge(new Edge(node, m, weight));
                          m.addEdge(new Edge(m, node, weight));
                      });
            }
        }
    }

    /**
     * Returns if the given point is on the grid.
     */
    private boolean isValid(Point p) {
        return p.row > 0 && p.row < rows - 1
            && p.col > 0 && p.col < cols - 1;
    }

    /**
     * Generates a new random graph that fits within the grid and returns the set of nodes.
     */
    private Set<Node> generateGraph(Random rand,
                                    Tile.Type targetType,
                                    Supplier<Integer> goldGenerator) {
        List<Node> nodes= new ArrayList<>();

        Set<Point> pointsSeen= new HashSet<>();
        Set<Point> openPoints= new HashSet<>();
        Queue<Node> frontier= new ArrayDeque<>();

        Point entrancePoint= getEntrancePoint(rand);
        Node entrance= new Node(new Tile(entrancePoint.row, entrancePoint.col, 0, Tile.Type.ENTRANCE), cols);
        nodes.add(entrance);

        pointsSeen.add(entrancePoint);
        openPoints.add(entrancePoint);
        frontier.add(entrance);
        while (!frontier.isEmpty()) {
            Node node= frontier.remove();
            Point p= new Point(node.getTile().getRow(), node.getTile().getColumn());

            // We want to make sure there's a way out if we can get one.
            // This will prevent stupid degenerate graphs.
            int existingExits= 0;
            List<Point> newExits= new ArrayList<>();
            for (Direction dir : Direction.values()) {
                Point newPt= dir.getPoint().add(p);
                if (isValid(newPt)) {
                    if (openPoints.contains(newPt)) {
                        existingExits++;
                    } else if (pointsSeen.add(newPt)) {
                        newExits.add(newPt);
                    }
                }
            }

            int nExits= newExits.size();
            if (nExits > 0) {
                double modifiedDensity;
                Point forcedExit;
                // Modify the density function so that the expected number of open exits
                // is the same even though we're forcing something to be open.
                if (existingExits < 2) {
                    modifiedDensity= (nExits == 1 ? 0.0 : (nExits * DENSITY - 1) / (nExits - 1));
                    forcedExit= newExits.get(rand.nextInt(newExits.size()));
                } else {
                    modifiedDensity= DENSITY;
                    forcedExit= null;
                }
                newExits.stream()
                        .filter((q) -> q.equals(forcedExit) || rand.nextDouble() < modifiedDensity)
                        .peek(openPoints::add)
                        .map((q) -> new Node(new Tile(q.row, q.col, goldGenerator.get(), Tile.Type.FLOOR), cols))
                        .peek(frontier::add)
                        .forEach(nodes::add);
            }
        }

        if (targetType != Tile.Type.ENTRANCE) {
            // Grab a random tile that's not the entrance and make it the target.
            int targetIdx= rand.nextInt(nodes.size() - 1) + 1;
            nodes.get(targetIdx).getTile().setType(targetType);
        }

        return Collections.unmodifiableSet(new HashSet<>(nodes));
    }

    /**
     * Randomly determines the entrance to the cavern (the only non-wall tile along an edge of the grid).
     */
    private Point getEntrancePoint(Random rand) {
        switch (rand.nextInt(4)) {
            case 0: // North wall
                return new Point(rand.nextInt(rows - 2) + 1, 0);
            case 1: // South wall
                return new Point(rand.nextInt(rows - 2) + 1, cols - 1);
            case 2: // West wall
                return new Point(0, rand.nextInt(cols - 2) + 1);
            case 3: // East wall
                return new Point(rows - 1, rand.nextInt(cols - 2) + 1);
            default:
                throw new IllegalStateException("Unexpected random value!");
        }
    }

    /**
     * Returns the number of open floor tiles in this cavern (this is the size of the graph).
     */
    public int numOpenTiles(){
        return graph.size();
    }

    /**
     * Returns the number of rows in the grid.
     */
    public int getRowCount() {
        return rows;
    }

    /**
     * Returns the number of columns in the grid.
     */
    public int getColumnCount() {
        return cols;
    }

    /**
     * Returns the set of all nodes in the graph.
     * This is an umodifiable view of the graph.
     */
    public Set<Node> getGraph() {
        return graph;
    }

    /**
     * Returns the node corresponding to the entrance to the cavern.
     */
    public Node getEntrance() {
        return entrance;
    }

    /**
     * Returns the target node in this cavern.
     */
    public Node getTarget() {
        return target;
    }

    /**
     * Returns the Tile information for a given row and column.
     * Preconditions: ({@code r}, {@code c}) must be in the grid.
     */
    public Tile getTileAt(int r, int c) {
        return tiles[r][c].getTile();
    }

    /**
     * Returns the node at the given column and row.
     * Preconditions: ({@code r}, {@code c}) must be in the grid.
     */
    public Node getNodeAt(int r, int c) {
        return tiles[r][c];
    }

    /**
     * Package-private implementation of Dijkstra's algorithm that returns only the minimum distance
     * between the given node and the target node for this cavern (no path).
     * Precondition: {@code start} must be a node in the graph of this cavern.
     */
    int minPathLengthToTarget(Node start) {
        Map<Long, Integer> pathWeights= new HashMap<>();
        InternalMinHeap<Node> heap= new InternalMinHeap<>();

        pathWeights.put(start.getId(), 0);
        heap.add(start, 0);
        while (!heap.isEmpty()) {
            Node n= heap.poll();
            if (n.equals(target)) {
                return pathWeights.get(n.getId());
            }

            int nWeight= pathWeights.get(n.getId());

            for (Edge e : n.getExits()) {
                Node m= e.getOther(n);
                int weightThroughN= nWeight + e.length();
                Integer existingWeight= pathWeights.get(m.getId());
                if (existingWeight == null) {
                    pathWeights.put(m.getId(), weightThroughN);
                    heap.add(m, weightThroughN);
                } else if (weightThroughN < existingWeight) {
                    pathWeights.put(m.getId(), weightThroughN);
                    heap.changePriority(m, weightThroughN);
                }
            }
        }
        throw new IllegalStateException("The above loop should always reach the desired location.");
    }

    /**
     * Serializes this cavern to a list of strings which can be written out to a file.
     * The list of strings can be converted back into a {@code Cavern} using {@code deserialize()}.
     */
    public List<String> serialize() {
        List<String> nodes= new ArrayList<>();
        nodes.add(rows + ":" + cols + ",trgt:" + target.getId());
        for (Node n : graph) {
            Tile t= n.getTile();
            String nodeStr= n.getId() + "," + t.getRow() + "," + t.getColumn() + "," + t.getGold() + "," + t.getType().name();

            String edges= n.getExits().stream()
                                      .map((e) -> e.getOther(n).getId() + "-" + e.length())
                                      .collect(Collectors.joining(","));
            nodes.add(nodeStr + "=" + edges);
        }
        return nodes;
    }

    /**
     * Takes a list of strings output by {@code serialize()} and converts it back into a {@code Cavern}.
     * Precondition: The list of strings is of the format output by {@code serialize()}.
     */
    public static Cavern deserialize(List<String> nodeStrList) {
        String extraInfo= nodeStrList.get(0);
        String[] infoParts= extraInfo.split(",");
        String[] dimensions= infoParts[0].split(":");
        int rows= Integer.parseInt(dimensions[0]);
        int cols= Integer.parseInt(dimensions[1]);
        long targetId= Long.parseLong(infoParts[1].split(":")[1]);

        Map<Long, Node> idToNode= new HashMap<>();
        for (String nodeStr : nodeStrList) {
            if (nodeStr.equals(extraInfo)) {
                continue;
            }

            String nodeInfo= nodeStr.substring(0, nodeStr.indexOf("="));
            String[] splitInfo= nodeInfo.split(",");

            long nodeId= Long.parseLong(splitInfo[0]);
            Node n= new Node(nodeId,
                             new Tile(Integer.parseInt(splitInfo[1]),
                                      Integer.parseInt(splitInfo[2]),
                                      Integer.parseInt(splitInfo[3]),
                                      Tile.Type.valueOf(splitInfo[4])));
            idToNode.put(nodeId, n);
        }

        Node[][] tiles= new Node[rows][cols];
        for (String nodeStr : nodeStrList) {
            // The first line is not a node, it's metadata, so skip it.
            if (nodeStr.equals(extraInfo)) {
                continue;
            }

            String[] nodeAndEdgeInfo= nodeStr.split("=");
            long nodeId= Long.parseLong(nodeAndEdgeInfo[0].split(",")[0]);

            Node n= idToNode.get(nodeId);
            tiles[n.getTile().getRow()][n.getTile().getColumn()]= n;
            for (String edgeStr : nodeAndEdgeInfo[1].split(",")) {
                String[] idAndWeight= edgeStr.split("-");
                long otherId= Long.parseLong(idAndWeight[0]);
                int weight= Integer.parseInt(idAndWeight[1]);
                n.addEdge(new Edge(n, idToNode.get(otherId), weight));
            }
        }

        for (int i= 0; i < rows; i++) {
            for (int j= 0; j < cols; j++) {
                if (tiles[i][j] == null) {
                    tiles[i][j]= new Node(new Tile(i, j, 0, Tile.Type.WALL), cols);
                }
            }
        }
        return new Cavern(new HashSet<>(idToNode.values()), tiles, idToNode.get(targetId));
    }
}
