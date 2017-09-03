package de.greyshine.utils.beta;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

public class Mapper {
	
	@SuppressWarnings("rawtypes")
	private final Map<MappingKey,Function> mappings = new HashMap<>();
	
	public Mapper() {
		this( true );
	}
	
	public Mapper(boolean inCreateDefaults) {
		if ( inCreateDefaults ) {
			initDefaults();
		}
	}

	private void initDefaults() {
		
		register( String.class , Boolean.class, (s)->Boolean.parseBoolean(s) );
		register( String.class , Short.class, (s)->Short.parseShort(s) );
		register( String.class , Integer.class, (s)->Integer.parseInt(s) );
		register( String.class , Double.class, (s)->Double.parseDouble(s) );
		register( String.class , Float.class, (s)->Float.parseFloat(s) );
		register( String.class , Character.class, (s)->s==null||s.length()<1?null:s.charAt(0) );
		register( String.class , BigInteger.class, (s)->new BigInteger( s ) );
		register( String.class , BigDecimal.class, (s)->new BigDecimal( s ) );
	}

	public void map(Object inSrcObject, Object inTargetObject, boolean inStrict) {
		
	}
	
	public <T> T map( Object inSrcObject, Class<T> inTargetClass) throws MappingException {
		return map( inSrcObject, inTargetClass, null, true );
	}

	public <T> T map( Object inSrcObject, Class<T> inTargetClass, boolean inStrict) throws MappingException {
		return map( inSrcObject, inTargetClass, null, inStrict );
	}
	
	@SuppressWarnings("unchecked")
	public <T> T[] mapArray(Object inSrcObject, Class<T> inTargetClass, T inDefault, boolean inStrict ) throws MappingException {
		
		if ( inTargetClass == null ) { return null; }
		else if ( inSrcObject == null ) { return (T[]) Array.newInstance( inTargetClass , 0); }
		else if ( !inSrcObject.getClass().isArray() ) {
			inSrcObject = new Object[] { inSrcObject };
		}
		
		final int l = Array.getLength( inSrcObject );
		final T[] theResultArray = (T[]) Array.newInstance( inTargetClass ,  l);

		for( int i=0; i < l; i++ ) {
			
			final Object theSrcObject = Array.get( inSrcObject , i);
			if ( theSrcObject == null ) {
				
				theResultArray[i] = null;
			
			} else if ( theSrcObject.getClass().isArray() ) {
				
				
				theResultArray[i] = (T) mapArray(inSrcObject, inTargetClass, inDefault, inStrict);
			
			} else {
			
				theResultArray[i] = map(theSrcObject, inTargetClass, inDefault, inStrict);
			}
		}
		
		return theResultArray;
		
	}
	
	public <T> T mapSafe( Object inSrcObject, Class<T> inTargetClass, T inDefault, boolean inStrict ) {
		
		try {
			
			return map( inSrcObject, inTargetClass, inDefault, inStrict );
			
		} catch (Exception e) {
			
			return inDefault;
		}
	}
	
	public <T> T map( Object inSrcObject, Class<T> inTargetClass, T inDefault, boolean inStrict ) throws MappingException {
		
		if ( inSrcObject == null || inTargetClass == null ) { return inDefault; }
		
		@SuppressWarnings("unchecked")
		final Function<Object,Object> theMapping = mappings.get( new MappingKey( inSrcObject.getClass(), inTargetClass) );
		
		if ( theMapping == null && inStrict == false ) {
			
			return inDefault;
		
		} else if ( theMapping == null ) {
			
			throw new MappingException( inSrcObject.getClass(), inTargetClass );
		}
		
		try {
			
			@SuppressWarnings("unchecked")
			final T theResult = (T) theMapping.apply( inSrcObject ); 

			return theResult == null ? inDefault : theResult;
			
		} catch (Exception e) {
			
			if ( inStrict == false ) {
				return inDefault;
			}
			
			throw new MappingException(inSrcObject.getClass(), inTargetClass, e);
		}
		
		
	}
	
	public <S,T> Mapper register( Class<S> inSrcClass, Class<T> inTargetClass, Function<S,T> inMappingFunction ) {
		
		mappings.put( new MappingKey(inSrcClass, inTargetClass) , inMappingFunction);
		
		return this;
	}
	
	private static class MappingKey {
		
		public final String key;
		
		private MappingKey(Class<?> sourceClass, Class<?> targetClass) {
			
			String srcKeyPart = sourceClass == null ? "?" : sourceClass.getTypeName();
			String trgKeyPart = targetClass == null ? "?" : targetClass.getTypeName();
			
			key = srcKeyPart+" > "+ trgKeyPart;
		}

		@Override
		public int hashCode() {
			return key.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			return this == obj || (obj instanceof MappingKey && key.equals( ((MappingKey)obj).key )); 
		}
	}
	
	public static class MappingException extends Exception {

		private static final long serialVersionUID = -2762500489888951625L;

		public final Class<?> sourceClass;
		public final Class<?> targetClass;

		public MappingException(Object inSource, Class<?> targetClass) {
		
			this( inSource, targetClass, null, null ) ;
		}

		public MappingException(Object inSource, Class<?> targetClass, Exception e ) {
			this( inSource, targetClass, null, e );
		}
		
		private static String createDefaultMessage(Object source, Class<?> targetClass) {
			return "mapping failure "+ source +" ("+ source == null ? "null" : source.getClass().getTypeName() +") to type "+ targetClass.getTypeName();
		}
		
		public MappingException(Object source, Class<?> targetClass, String inMessage, Throwable inCause) {
			super( inMessage != null ? inMessage : createDefaultMessage(source, targetClass), inCause );
			
			this.sourceClass = source == null ? null : source.getClass();
			this.targetClass = targetClass;
		}

		@Override
		public String toString() {
			return "MappingException [sourceClass=" + sourceClass + ", targetClass=" + targetClass + ", message="+ getMessage() +"]";
		}
	}
	
	public static Function<String, BigDecimal> S2BD = (v)->{ return new BigDecimal( v ); };
	
	
}
