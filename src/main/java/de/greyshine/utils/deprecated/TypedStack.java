package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.List;

public class TypedStack<T> {

	private final List<T> list = new ArrayList<T>();

	public T getFirst() {

		return list.isEmpty() ? null : list.get(0);
	}

	public void addFirst(T inValue) {

		list.add(0, inValue);
	}

	public void addLast(T inValue) {

		list.add(list.size(), inValue);
	}

	public T getLast() {
		
		return list.isEmpty() ? null : list.get(list.size() - 1);
	}

	public T removeFirst() {

		return list.isEmpty() ? null : list.remove(0);
	}

	public T removeLast() {

		return list.isEmpty() ? null : list.remove(list.size() - 1);
	}

	public boolean isEmpty() {
		
		return list.isEmpty();
	}
}
