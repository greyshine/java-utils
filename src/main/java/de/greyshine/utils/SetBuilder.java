package de.greyshine.utils;

import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;

public class SetBuilder<T> {

	public SetBuilder() {}
	
	private Set<T> set = new LinkedHashSet<T>();
	
	public SetBuilder<T> add(@SuppressWarnings("unchecked") T... inValues) {
		
		if ( inValues == null ) { return this; }
		
		for (T aValue : inValues) {
			
			set.add( aValue );
		}
		
		return this;
	}
	
	public Set<T> create() {
		return create( false );
	}

	public Set<T> create(boolean inUnmodifiable) {
		
		return !inUnmodifiable ? new LinkedHashSet<T>( set ) : Collections.unmodifiableSet( set ) ;
	}
	
	@SafeVarargs
	public static <T> Set<T> create(T... inValues) {
		
		final Set<T> s = new HashSet<T>();
		
		if ( inValues != null ) {
			
			for (int i = 0, l=inValues.length; i < l; i++) {
				s.add( inValues[i] );
			}
		}
		
		return s;
		
	}
}
