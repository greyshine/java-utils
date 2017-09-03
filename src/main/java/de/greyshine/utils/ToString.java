package de.greyshine.utils;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;

public class ToString {

	private Map<Class<?>, Function<Object, String>> tostringers = new LinkedHashMap<>();
	
	private static final ThreadLocal<StringBuilder> TL_STRINGBUFFER = new ThreadLocal<StringBuilder>() {
		@Override
		protected StringBuilder initialValue() {
			return new StringBuilder();
		}
	};  

	private final Function<Object, String> TOSTRINGER_DEFAULT = new Function<Object,String>() {
		
		@Override
		public final String apply(Object t) {
			
			if ( t == null ) { return "null"; }
			else if ( t instanceof String || t.getClass().isPrimitive()
					|| t instanceof Boolean
					|| t instanceof Byte
					|| t instanceof Character
					|| t instanceof Short
					|| t instanceof Integer
					|| t instanceof Long
					|| t instanceof Double
					|| t instanceof Float ) {
				return String.valueOf( t );
			}
			else if ( t instanceof BigDecimal ) {
				
				return ((BigDecimal)t).toPlainString();
			}
			
			final StringBuilder s = TL_STRINGBUFFER.get();
			s.setLength( 0 );
			s.append( t.getClass().getName() ).append( " [hash=" ).append( t.hashCode() ).append( ']' );
			return s.toString();
		}
		
		@Override
		public final String toString() {
			return ToString.class.getTypeName() +".TOSTRINGER_DEFAULT";
		}
	};

	private final Function<Object, String> TOSTRINGER_ARRAY = new Function<Object,String>() {

		@Override
		public String apply(Object t) {
			
			if ( t == null || !t.getClass().isArray() ) { return TOSTRINGER_DEFAULT.apply(t); }
			
			final StringBuilder s = new StringBuilder();
			s.setLength( 0 );
			
			final int l = Array.getLength( t );
			s.append( t.getClass().getComponentType().getTypeName() ).append( ':' );
			s.append( l ).append("[");
			
			for(int i=0; i<l; i++) {

				s.append( ToString.this.toString( Array.get(t, i) ) );
				
				if ( i+1 < l ) {
					s.append( ", " );
				}
			}
			
			return s.append( ']' ).toString();
		}
	};
	
	public final Function<Object, String> TOSTRINGER_THROWABLE = new Function<Object,String>() {
		@Override
		public final String apply(Object o) {
			
			if ( !(o instanceof Throwable) ) { return TOSTRINGER_DEFAULT.apply(o); }
			
			final StringBuilder s = TL_STRINGBUFFER.get();
			s.setLength( 0 );
			final Throwable t = (Throwable) o;
			s.append( t.getClass().getName() ).append( " [message=" ).append( t.getMessage() );
			
			String part = t.getLocalizedMessage();
			if ( Utils.isNotBlank( part ) && !part.equals( t.getMessage() ) ){
				s.append( ", localizedMessage=" ).append( part );
			}
			
			final Throwable cause = t.getCause();
			if ( cause != null && cause != t ) {
				s.append( ", cause=" ).append( cause.getClass().getName() );
			}
			
			return s.append( ']' ).toString();
		}
	};
	
	public final Function<Object, String> TOSTRINGER_JSON = new Function<Object,String>() {
		
		private final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
		
		@Override
		public final String apply(Object inJson) {
			
			inJson = inJson == null ? JsonNull.INSTANCE : inJson;
			
			if ( !(inJson instanceof JsonElement) ) { return TOSTRINGER_DEFAULT.apply( inJson ); }
			
			return GSON.toJson( inJson );
		}
	};
	
	{
		tostringers.put( Throwable.class , TOSTRINGER_THROWABLE);
	}
	
	public String toString(Object inObject) {
		
		try {
			
			return lookup( inObject ).apply( inObject );
			
		} catch (Exception e) {
			
			return TOSTRINGER_DEFAULT.apply( inObject );
		}
	}
	
	public ToString register( Class<?> inClass, Function<Object,String> inFunction ) {
		
		if ( inClass == null || inFunction == null) { return this; }
		
		tostringers.put( inClass , inFunction);
		
		return this;
		
	}

	private Function<Object,String> lookup(Object inObject) {
		
		if ( inObject == null ) { return TOSTRINGER_DEFAULT; }
		
		final Function<Object,String> theFunction = tostringers.get( inObject.getClass() );
		
		if ( theFunction != null ) { return theFunction; }
		
		final Wrapper<Function<Object,String>> theResultWrapper = new Wrapper<>(  );
		
		ReflectionUtils.traversClassHierarchy( inObject.getClass(),  new IClassHandler() {
			
			@Override
			public boolean handle(Class<?> inClass) {
				
				//System.out.println( "check "+ inClass );
				//System.out.println( "  ? "+ tostringers.get( inClass ) );
				
				theResultWrapper.set( tostringers.get( inClass ) );
				
				return theResultWrapper.isNull();
			}
		} );
		
		if ( theResultWrapper.isNull() && inObject.getClass().isArray() ) {
			
			theResultWrapper.set( TOSTRINGER_ARRAY );
		} 
		
		return theResultWrapper.getDefault( TOSTRINGER_DEFAULT );
	}

	@Override
	public final String toString() {
		return TOSTRINGER_DEFAULT.apply( this );
	}

}
