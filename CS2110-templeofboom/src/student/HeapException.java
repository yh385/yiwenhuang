package student;
public class HeapException extends RuntimeException {
    
    /** Constructor: an instance with message m*/
    public HeapException(String m) {
        super(m);
    }
    
    /** Constructor: an instance with no message */
    public HeapException() {
        super();
    }
}
