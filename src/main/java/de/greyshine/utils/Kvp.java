package de.greyshine.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Simple key value wrapper.
 * 
 *
 * @param <S>
 * @param <T>
 */
public class Kvp<S, T> {
	
	@SuppressWarnings("rawtypes")
	public static final Kvp[] EMPTY_ARRAY = new Kvp[0];
	@SuppressWarnings("rawtypes")
	public static final List<Kvp> EMPTY_COLLECTION = Collections.unmodifiableList(new ArrayList<Kvp>());

	public final S key;
	public T value;
	
	public Kvp(S key, T value) {

		this.key = key;
		this.value = value;
	}

	public static <S, T> Kvp<S, T> create(S s, T t) {

		return new Kvp<S, T>(s, t);
	}

	public S getKey() {
		return key;
	}

	public T getValue() {
		return value;
	}

	public T setValue(T inValue) {
		final T theOldValue = value;
		value = inValue;
		return theOldValue;
	}
	
	public boolean isNullValue() {
		return value == null;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((key == null)   ? 0 : key.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("rawtypes")
		final Kvp other = (Kvp) obj;
		if (key == null) {
			if (other.key != null)
				return false;
		} else if (!key.equals(other.key))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return key + "=" + value;
	}
}
