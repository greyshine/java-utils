package de.greyshine.utils.beta.objectfilestorage;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import de.greyshine.utils.Utils;

public class Cache<T> {
	
	/**
	 * <id,object>
	 */
	private Map<String,T> cache = new HashMap<>();
	
	private ILoader<T> loader;
	
	public Cache( ILoader<T> inLoader ) {
		
		if ( inLoader == null ) { throw new IllegalArgumentException("loader must not be null"); }

		loader = inLoader;
	}
	
	public static interface ILoader<T> {
		T load(String inId) throws IOException;
	}
	
	public T getObject(String inId) {
		
		if ( inId == null ) { return null; }

		synchronized ( cache ) {
			
			T theObject = cache.get(inId);
			
			if ( theObject != null ) { return theObject; }
			
			try {
				
				theObject = loader.load(inId);
				
			} catch ( Exception e ) {
				
				throw Utils.toRuntimeException(e);
			}

			cache.put( inId , theObject );
			
			return theObject;
		}
	}

	public void remove(String inId) {
	
		synchronized ( cache ) {
			cache.remove(inId);
		}
	}

	public void removeValue(T inObject) {
		
		synchronized ( cache ) {
			
			new ArrayList<>( cache.entrySet() ).stream()//
			.filter( inEntry->{ return inEntry.getValue() == inObject; } )//
			.forEach( inEntry->{

				cache.remove( inEntry.getKey() );
			} );
		}
	}
	
	public void setObject( String inId, T inObject ) {

		setObject(inId, inObject, null);
	}

	public void setObject( String inId, T inObject, Utils.IExecuter<T> inExecuter ) {
		
		if ( inId == null ) {
			
			return;
		} 
		
		
		synchronized ( cache ) {
			
			if ( inExecuter != null ) {
				
				Utils.execute( inExecuter );
			}
			
			cache.put(inId, inObject);
		}
	}
	
	public int size() {
		synchronized (cache) {
			return cache.size();
		}
	}
	
	public long getMemoryUsage() {
		
		AtomicLong theSize = new AtomicLong(0L);
		
		synchronized (cache) {
			
			cache.values().parallelStream().forEach( o->{
				
				theSize.addAndGet( Utils.getMemoryUsage( o ) );
		
			} );

		}
		
		return theSize.get();
	}

	@SuppressWarnings("unchecked")
	public <T> T getObjectCasted(String inId) {
		
		final Object theObject = getObject(inId); 
		
		try {
			
			return (T)theObject;
			
		} catch (Exception e) {
			// intended swallow
		}
		
		return null;
	}
	
}
