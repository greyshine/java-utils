package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.OutputStream;

/**
 * Allows to stream content to an {@link OutputStream}
 */
public interface IStreamable {

	/**
	 * @param inOut
	 * @return the bytes streams or <code>null</code> if information is not
	 *         available
	 * @throws IOException
	 */
	Long stream(OutputStream inOut) throws IOException;
}
