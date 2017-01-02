package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.List;

public class Stack {

	private List<Object> stack = new ArrayList<Object>();

	public void push(Object inObject) {

		stack.add(inObject);
	}

	public Object pop() {

		return stack.isEmpty() ? null : stack.remove(0);
	}

	public <T> T pop(Class<T> inClass) {

		final T theObject = peek(inClass);

		if (theObject != null) {

			for (int i = 0, l = stack.size(); i < l; i++) {
				
				if (stack.get(i) == theObject) {

					stack.remove(i);
					break;
				}
			}
		}
		
		return theObject;
	}

	public Object peek() {

		return stack.isEmpty() ? null : stack.get(0);
	}

	@SuppressWarnings("unchecked")
	public <T> T peek(Class<T> inClass) {

		if (inClass == null) {
			return null;
		}

		for (final Object anElement : stack) {

			if (anElement != null && inClass.isAssignableFrom(anElement.getClass())) {

				return (T) anElement;
			}
		}

		return null;
	}


}
