package de.greyshine.utils;

import java.util.Map;

/**
 * Handler for handling items of a {@link Map}
 * 
 * 
 * @param <S>
 * @param <T>
 */
public interface IMapHandler<S, T> {

	boolean handle(S inKey, T inValue);

	boolean handleException(S inKey, Exception inException, T inValue);

	void done(int inHandles);
}