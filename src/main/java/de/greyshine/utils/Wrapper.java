package de.greyshine.utils;

import java.util.function.Consumer;
import java.util.function.Function;

public class Wrapper<T> {
	
	public T value;
	
	public Wrapper() {}
	public Wrapper(T inValue) {
		value=inValue;
	}

	public T get() {
		return value;
	}
	
	public void set(T value) {
		this.value = value;
	}
	
	public T getValue() {
		return value;
	}
	
	public void setValue(T value) {
		this.value = value;
	}
	
	/**
	 * @param inDefault
	 * @return default value if null
	 */
	public T getDefault(T inDefault) {
		return value == null ? inDefault : value;
	}
	
	public boolean isNull() {
		return value == null;
	}
	
	public boolean isNotNull() {
		return value != null;
	}
	
	public boolean isBlankString() {
		return value instanceof String && ((String)value).isEmpty();
	}
	
	public boolean isNotBlankString() {
		return value instanceof String && !((String)value).isEmpty();
	}
	
	public void consume(Consumer<? super T> inConsumer) {
		if ( inConsumer != null ) {
			inConsumer.accept( value );
		}
	}
	
	@SuppressWarnings("unchecked")
	public <U> U map( Function<? super T, ? super U> inFunction ) {
		if ( inFunction == null ) {
			return null;
		} 
		return (U) inFunction.apply( value );
	}
	
	public String toString() {
		return getClass().getSimpleName() +" [isNull="+ isNull() +", value="+ value +"]";
	}
}