package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.InputStream;


public class AutoCloseInputStream extends InputStream {

	private final InputStream is;

	public AutoCloseInputStream(InputStream is) {

		if (is == null) {
			throw new IllegalArgumentException("InputStream must not be null");
		}

		this.is = is;


	}

	@Override
	public int read() throws IOException {
		
		final int r = is == null ? -1 : is.read();
		
		if (r == -1) {
			
			Utils.close(is);
		}
		
		return r;
	}

	@Override
	public int available() throws IOException {

		return is.available();
	}

	@Override
	public void close() throws IOException {
		is.close();
	}

	@Override
	public synchronized void mark(int readlimit) {
		is.mark(readlimit);
	}

	@Override
	public synchronized void reset() throws IOException {
		is.reset();
	}

	@Override
	public boolean markSupported() {
		return is.markSupported();
	}

}
