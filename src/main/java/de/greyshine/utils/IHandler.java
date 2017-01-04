package de.greyshine.utils;

/**
 * Interface indicating to handle an object and re-inform about further processing
 * 
 * @param <T>
 */
public interface IHandler<T> {

	/**
	 * 
	 * @param inObject
	 * @param inIdx
	 * @return <code>true</code> when handling should be continued, otherwise <code>false</code>
	 */
	boolean handle(T inObject, int inIdx);

	/**
	 * 
	 * @param inIdx
	 * @param inException
	 * @param inObject
	 * @return <code>true</code> when handling should be continued, otherwise <code>false</code>
	 */
	boolean handleException(int inIdx, Exception inException, T inObject);

	void done(int inHandles);
}