package student;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import game.EscapeState;
import game.ExploreState;
import game.Explorer;
import game.Node;
import game.NodeStatus;

public class Tennessee extends Explorer {
    /** Get to the orb in as few steps as possible. Once you get there, 
     * you must return from the function in order to pick
     * it up. If you continue to move after finding the orb rather 
     * than returning, it will not count.
     * If you return from this function while not standing on top of the orb, 
     * it will count as a failure.
     * 
     * There is no limit to how many steps you can take, but you will receive
     * a score bonus multiplier for finding the orb in fewer steps.
     * 
     * At every step, you know only your current tile's ID and the ID of all 
     * open neighbor tiles, as well as the distance to the orb at each of these tiles
     * (ignoring walls and obstacles). 
     * 
     * In order to get information about the current state, use functions
     * currentLocation(), neighbors(), and distanceToOrb() in ExploreState.
     * You know you are standing on the orb when distanceToOrb() is 0.
     * 
     * Use function moveTo(long id) in ExploreState to move to a neighboring 
     * tile by its ID. Doing this will change state to reflect your new position.
     * 
     * A suggested first implementation that will always find the orb, but likely won't
     * receive a large bonus multiplier, is a depth-first search.*/
    @Override public void getOrb(ExploreState state) {
        //TODO : Get the orb
    	HashSet<Long> visited = new HashSet<Long>();
    	optimalChoice(state, visited);
    }
    
    /** Move to the neighbor that has not been visited and has the shortest Manhattan
     * distance to the orb.*/
    private void optimalChoice(ExploreState state, HashSet<Long> visited) {
    	List<NodeStatus> neighbors = new ArrayList<NodeStatus>(state.neighbors());
    	Collections.sort(neighbors); //neighbors sorted by shortest distance to orb
    	//base case
    	if (state.distanceToOrb()==0) return;
    	//recursive case
    	visited.add(state.currentLocation());
    	Long previous = state.currentLocation();
    	for (NodeStatus status: neighbors){ //go to the neighbor if it has not been visited
    		if(!visited.contains(status.getId())) {
    			state.moveTo(status.getId());
    			optimalChoice(state,visited);
    	    	if (state.distanceToOrb()==0) return;
    			state.moveTo(previous); //move back if reached a dead end
    		}
    	}
    }
    
    /** Get out the cavern before the ceiling collapses, trying to collect as much
     * gold as possible along the way. Your solution must ALWAYS get out before time runs
     * out, and this should be prioritized above collecting gold.
     * 
     * You now have access to the entire underlying graph, which can be accessed through EscapeState.
     * currentNode() and getExit() will return Node objects of interest, and getNodes()
     * will return a collection of all nodes on the graph. 
     * 
     * Note that the cavern will collapse in the number of steps given by stepsRemaining(),
     * and for each step this number is decremented by the weight of the edge taken. You can use
     * stepsRemaining() to get the time still remaining, seizeGold() to pick up any gold
     * on your current tile (this will fail if no such gold exists), and moveTo() to move
     * to a destination node adjacent to your current node.
     * 
     * You must return from this function while standing at the exit. Failing to do so before time
     * runs out or returning from the wrong location will be considered a failed run.
     * 
     * You will always have enough time to escape using the shortest path from the starting
     * position to the exit, although this will not collect much gold. For this reason, using 
     * Dijkstra's to plot the shortest path to the exit is a good starting solution. */
    @Override public void getOut(EscapeState state) {
        //TODO: Escape from the cavern before time runs out
    		grabGold(state);
    }
    
    /** Returns an ordered set of tiles with golds.
     * The set is sorted such that the gold tile that comes first is the one
     * that can form a shortest path from current location to the tile
     * with the most amount of gold along the path. */
    private TreeSet<Node> getGoldSet(EscapeState state) {
    	TreeSet<Node> goldTiles = new TreeSet<Node>(new Comparator<Node>() {
			@Override
			public int compare(Node o1, Node o2) {
		    	List<Node> patho1= Paths.dijkstra(state.currentNode(), o1);
		    	List<Node> patho2= Paths.dijkstra(state.currentNode(), o2);
		    	if(Paths.pathGoldAmount(patho1)<Paths.pathGoldAmount(patho2)) return 1;
		    	else if (Paths.pathGoldAmount(patho1)>Paths.pathGoldAmount(patho2)) return -1;
		    	return 0;
			}
    	});
    	goldTiles.addAll(Paths.allGolds(state)); //sorted set of all gold tiles
    	return goldTiles;
    }
    
    /** Returns an optimal path, from current location to a gold, that has the most amount
     * of gold and will guarantee getting to the exit before running out of
     * steps remaining. */
    private List<Node> getBestValidPath(EscapeState state){
    	//the set of gold tiles at current state
    	TreeSet<Node> goldTiles = getGoldSet(state); 
    	//find the optimal path
    	Iterator<Node> itr = goldTiles.iterator();
        Node gold= itr.next();  // First node in the goldSet
        while (itr.hasNext()) {
        	List<Node> path = Paths.dijkstra(state.currentNode(), gold);
        	//make sure after taking the path one can still exit in time
        	if (roundtrip(state,gold)<=state.stepsRemaining()) { 
        		return path;
        	}
        	gold = itr.next();
        }
        return null;
    }
    
    /** Grabs the gold along the path with most amount of gold.*/
    private void grabGold(EscapeState state){
    	List<Node> path = getBestValidPath(state);
    	//while there is still a best path that guarantees exit in time
    	while(path!= null){
			for (Node tile : path.subList(1, path.size())){
				pickUp(state.currentNode(),state);
				state.moveTo(tile);
			}
			path = getBestValidPath(state);
		}
    	//go to the exit
		List<Node> pathExit = Paths.dijkstra(state.currentNode(), state.getExit());
		for (Node tile : pathExit.subList(1, pathExit.size())){
			pickUp(state.currentNode(),state);
			state.moveTo(tile);
		}
    }
    
    /** Return the length of the path, going from current location to the move 
     * then to the exit.
     * Precondition: move is not null */
    private int roundtrip(EscapeState state, Node move){
    	List<Node> path = Paths.dijkstra(state.currentNode(), move);
    	List<Node> path2 = Paths.dijkstra(move, state.getExit());
    	return Paths.pathLength(path)+Paths.pathLength(path2);
    }
    
    /** Pick up gold from the tile if there is gold on the tile. */
    private void pickUp(Node tile, EscapeState state){
    	try{
    		state.seizeGold();
    	} catch(IllegalStateException e){
    		return;
    	}
    }
}

