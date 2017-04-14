package de.greyshine.utils;

public interface IClassHandler {

	/**
	 * 
	 * @param inClass
	 * @return <code>true</code> in order to keep on traversing, <code>false</code> in order to stop the traversal.
	 */
	boolean handle(Class<?> inClass);
}