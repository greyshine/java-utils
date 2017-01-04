package de.greyshine.utils;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class MapBuilder<K, V> {

	private Map<K, V> map;

	public MapBuilder() {

		this(1);
	}

	public MapBuilder(int inSize) {

		this(new HashMap<K, V>(inSize < 1 ? 1 : inSize));
	}

	public MapBuilder(Map<K, V> inMap) {

		if (inMap == null) {

			throw new IllegalArgumentException("map must not be null");
		}

		map = inMap;
	}

	public MapBuilder(K inKey, V inValue) {

		this();
		put(inKey, inValue);
	}

	public static <K, V> MapBuilder<K, V> create(K inKey, V inValue) {

		return new MapBuilder<K, V>(inKey, inValue);
	}

	public static <K, V> Map<K, V> build(K inKey, V inValue) {

		return new MapBuilder<K, V>(inKey, inValue).map;
	}

	public MapBuilder<K, V> put(K inKey, V inValue) {

		map.put(inKey, inValue);
		return this;
	}

	public MapBuilder<K, V> remove(K inKey) {

		map.remove(inKey);
		return this;
	}

	public Set<Map.Entry<K, V>> getEntries() {

		return map.entrySet();
	}

	public Map<K, V> get() {

		return map;
	}
	
	/**
	 * @param inModifiable whether to build an unmodifyable map
	 * @return the newly created {@link Map}
	 */
	public Map<K, V> get(boolean inModifiable) {
		return inModifiable ? map : Collections.unmodifiableMap( map );
	}
	
	public Set<K> getKeys() {

		return map.keySet();
	}

	public Set<V> getValues() {

		return (Set<V>) map.values();
	}

	public void forEach(final IHandler<K, V> inHandler) throws Exception {

		if (inHandler == null) {
			return;
		}

		final int theSize = map.size();
		int theItem = 0;
		
		for (final Map.Entry<K, V> anEntry : map.entrySet()) {

			inHandler.handle(theItem++, theSize, anEntry.getKey(), anEntry.getValue());
		}
	}
	
	public interface IHandler<K, V> {

		void handle(int inItem, int inSize, K inKey, V inValue) throws Exception;
	}


}
