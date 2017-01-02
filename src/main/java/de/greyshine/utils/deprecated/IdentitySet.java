package de.greyshine.utils.deprecated;

import java.io.Serializable;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;

public class IdentitySet<E> extends AbstractSet<E> implements Set<E>, Cloneable, Serializable {

	private static final long serialVersionUID = 4198191420292951942L;

	private transient HashMap<Key,E> map;
	
	private class Key {
		
		final Object object;

		private Key(Object object) {
			this.object = object;
		}

		@Override
		public int hashCode() {
			return object == null ? 0 : object.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			
			if ( !(obj instanceof IdentitySet.Key) ) { return false; }
			
			@SuppressWarnings("unchecked")
			final Key theOther = (Key) obj;
			
			if ( object == null && theOther.object != null ) { return false; }
			if ( object != null && theOther.object == null ) { return false; }
			
			return object == theOther.object;
		}

		@Override
		protected Object clone() throws CloneNotSupportedException {
			return new Key( object );
		}
	}

    public IdentitySet() {
        this(16);
    }

	public IdentitySet(int inInitialSize) {
		map = new HashMap<>( inInitialSize < 1 ? 0 : inInitialSize );
	}

	@SuppressWarnings("unchecked")
	@Override
	public Iterator<E> iterator() {
		return (Iterator<E>) map.values();
	}

	@Override
	public int size() {
		return map.size();
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		return map.containsKey( new Key(o) );
	}

	@Override
	public Object[] toArray() {
		return map.values().toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return map.values().toArray( a );
	}

	@Override
	public boolean add(E e) {
		
		final Key k = new Key(e);
		if ( map.containsKey( k ) ) {

			return false;
		
		} else {
			
			map.put( k , e);
			return true;
		}
	}

	@Override
	public boolean remove(Object o) {
		
		final Key k = new Key(o);
		if ( !map.containsKey( k ) ) {

			return false;
		
		} else {
			
			map.remove( k );
			return true;
		}
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {

		final int size = map.size();
		super.addAll(c);
		return size != map.size();
	}

	@Override
	public void clear() {
		map.clear();
	}
	
	   /**
     * Save the state of this <tt>HashSet</tt> instance to a stream (that is,
     * serialize it).
     *
     * @serialData The capacity of the backing <tt>HashMap</tt> instance
     *             (int), and its load factor (float) are emitted, followed by
     *             the size of the set (the number of elements it contains)
     *             (int), followed by all of its elements (each an Object) in
     *             no particular order.
     */
    private void writeObject(java.io.ObjectOutputStream s)
        throws java.io.IOException {
        // Write out any hidden serialization magic
        s.defaultWriteObject();

        // Write out size
        s.writeInt(map.size());

        // Write out all elements in the proper order.
        for (E e : map.values())
            s.writeObject(e);
    }

    /**
     * Reconstitute the <tt>HashSet</tt> instance from a stream (that is,
     * deserialize it).
     */
    @SuppressWarnings("unchecked")
	private void readObject(java.io.ObjectInputStream s)
        throws java.io.IOException, ClassNotFoundException {
        // Read in any hidden serialization magic
        s.defaultReadObject();

        // Read in size
        int size = s.readInt();
        
        map.clear();

        // Read in all elements in the proper order.
        for (int i=0; i<size; i++) {
        	
        	final Key k = new Key( s.readObject() );
            map.put( k , (E) k.object);
        }
    }

}
