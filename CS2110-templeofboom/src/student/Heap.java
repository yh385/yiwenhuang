package student;
/* Time spent on a6:  hh hours and mm minutes.

 * Name(s): gries
 * Netid(s): djg17
 * What I thought about this assignment: Neat!
 * 
 *
 *
 */

import java.util.*;

/** An instance is a min-heap of distinct elements of type E.
 *  Priorities are double values. Since it's a min-heap, the value
 *  with the smallest priority is at the root of the heap. */
public class Heap<E>{

    private int size; // number of elements in the heap

    /** The heap invariant is given below. Note that / denotes int division.
     * 
     *  b[0..size-1] is viewed as a min-heap: 
     *  1. Each array element in b[0..size-1] contains a value of the heap.
     *  2. The children of each b[i] are b[2i+1] and b[2i+2].
     *  3. The parent of each b[i] (except b[0]) is b[(i-1)/2].
     *  4. The priority of the parent of each b[i] is <= the priority of b[i].
     *  5. Priorities for the b[i] used for the comparison in point 4
     *     are given in map. map contains one entry for each element of
     *     the heap, so map and b have the same size.
     *     For each element e in the heap, the map entry contains in the
     *     Prindex object the priority of e and its index in b.
     */
    private ArrayList<E> b;
    private HashMap<E, Prindex> map= new HashMap<E, Prindex>();

    /** Constructor: an empty heap. */
    public Heap() {
        b= new ArrayList<E>();
    }

    /** Return a string that gives this heap, in the format:
     * [item0:priority0, item1:priority1, ..., item(N-1):priority(N-1)]
     * Thus, the list is delimited by '['  and ']' and ", " (i.e. a
     * comma and a space char) separate adjacent items. */
    @Override public String toString() {
        String s= "[";
        for (E t : b) {
            if (s.length() > 1) {
                s = s + ", ";
            }
            s = s + t + ":" + map.get(t).priority;
        }
        return s + "]";
    }

    /** Return a string that gives the priorities in this heap,
     * in the format: [priority0, priority1, ..., priority(N-1)]
     * Thus, the list is delimited by '['  and ']' and ", " (i.e. a
     * comma and a space char) separate adjacent items. */
    public String toStringPriorities() {
        String s= "[";
        for (E t : b) {
            if (s.length() > 1) {
                s = s + ", ";
            }
            s = s + map.get(t).priority;
        }
        return s + "]";
    }

    /** Return the number of elements in this heap.
     *  This operation takes constant time. */
    public int size() {
        return size;
    }

    /** Add e with priority p to the heap.
     *  Throw an illegalArgumentException if e is already in the heap.
     *  The expected time is logarithmic and the worst-case time is linear
     *  in the size of the heap. */
    public void add(E e, double p) throws IllegalArgumentException {
        // TODO 1: Do add and bubbleUp together.
        // This is the ONLY method that should create a Prindex object.
        if (map.containsKey(e)) {
            throw new IllegalArgumentException("e is already in priority queue");
        }

        map.put(e, new Prindex(size, p));
        b.add(e);
        size= size + 1;
        bubbleUp(size-1);
    }

    /** Return the element of this heap with lowest priority, without
     *  changing the heap. This operation takes constant time.
     *  Throw a HeapException if the heap is empty. */
    public E peek() {
        // TODO 2: Do peek.
        if (size <= 0) throw new HeapException("heap is empty");
        return b.get(0);
    }

    /** Remove and return the element of this heap with lowest priority.
     *  The expected time is logarithmic and the worst-case time is linear
     *  in the size of the heap.
     *  Throw a HeapException if the heap is empty. */
    public E poll() {
        // TODO 3: Do poll and bubbleDown together.
        if (size <= 0) throw new HeapException("heap is empty");

        E val= b.get(0);
        map.remove(val);

        if (size == 1) {
            b.remove(0);
            size= 0;
            return val;
        }

        // At least 2 elements in queue
        b.set(0, b.get(size-1));
        map.get(b.get(0)).index= 0;
        b.remove(size-1);
        size= size - 1;

        bubbleDown(0);
        return val;
    }

    /** Change the priority of element e to p.
     *  The expected time is logarithmic and the worst-case time is linear
     *  in the size of the heap.
     *  Throw an IllegalArgumentException if e is not in the heap. */
    public void changePriority(E e, double p) {
        // TODO  4: Do updatePriority.
        Prindex info= map.get(e);
        if (info == null)
            throw new IllegalArgumentException("e is not in the priority queue");
        if (p > info.priority) {
            info.priority= p;
            bubbleDown(info.index);
        } else {
            info.priority= p;
            bubbleUp(info.index);
        }
    }


    /** Bubble b[k] up in heap to its right place.
     *  Precondition: Priority of every b[i] >= its parent's priority
     *                except perhaps for b[k] */
    private void bubbleUp(int k) {
        // TODO  Do add and bubbleUp together.

        E bk= b.get(k);
        Prindex bkInfo= map.get(bk);
        // Inv: bk = b[k]  AND  bkInfo is b[k]'s Prindex object AND
        //      Priority of very b[i] >= its parent's priority except perhaps for b[k]
        while (k > 0) {
            int p= (k-1) / 2; // p is k's parent
            E bp= b.get(p);
            Prindex bpInfo= map.get(bp);
            if (bkInfo.priority >= bpInfo.priority) return;

            //  Swap b[k] and b[p]
            b.set(p, bk);
            b.set(k, bp);
            bpInfo.index= k;
            bkInfo.index= p;

            k= p;
        }
    }

    /** Bubble b[k] down in heap until it finds the right place.
     *  Precondition: Each b[i]'s priority <= its childrens' priorities 
     *                except perhaps for b[k] */
    private void bubbleDown(int k) {
        // TODO 3: Do poll and bubbleDown together.

        // Throughout, k, bk, and bkInfo describe element k
        E bk= b.get(k);
        Prindex bkInfo= map.get(bk);

        // Invariant: bk = b[k], and bkInfo is b[k]'s Prindex object AND
        //    Priority of every b[i] <= its childrens' priorities except perhaps for b[k]
        while (2*k+1 < size) {
            int c= smallerChildOf(k);
            E bc= b.get(c);
            Prindex bcInfo= map.get(bc);

            if (bkInfo.priority <= bcInfo.priority) return;

            b.set(k, bc);
            b.set(c, bk);
            bcInfo.index= k;
            bkInfo.index= c;

            k= c;
        }
    }

    /** Return the index of the smaller child of b[n]
     *  Precondition: left child exists: 2n+1 < size of heap */
    private int smallerChildOf(int n) {
        int lChild= 2*n + 1;
        if (lChild + 1  ==  size) return lChild;

        double lchildPriority= map.get(b.get(lChild)).priority;
        double rchildPriority= map.get(b.get(lChild+1)).priority;
        if (lchildPriority < rchildPriority)
            return lChild;
        return lChild+1;
    }

    /** An instance contains the priority and index an element of the heap. */
    private static class Prindex {
        private int index;  // index of this element in map
        private double priority; // priority of this element

        /** Constructor: an instance in b[i] with priority p. */
        private Prindex(int i, double p) {
            index= i;
            priority= p;
        }

    }
}