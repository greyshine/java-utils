package de.greyshine.utils.deprecated;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Allows null-safe concatenation of all kinds of byte streams.
 * 
 * @author greyshine
 */
public class ArrayInputStream extends InputStream {

	private final List<InputStream> streams;
	private int currentIndex = 0;
	private InputStream currentStream;
	
	public ArrayInputStream(byte[][] inBytess) {

		if (inBytess != null) {

			streams = new ArrayList<InputStream>(inBytess.length);

			for (int i = 0; i < inBytess.length; i++) {

				if (inBytess[i] != null) {

					streams.add(new ByteArrayInputStream(inBytess[i]));
				}
			}

		} else {

			streams = Collections.emptyList();
			currentStream = null;
		}
	}

	public ArrayInputStream(final InputStream... inIss) {

		if (inIss != null) {

			streams = new ArrayList<InputStream>(inIss.length);

			for (final InputStream anIs : inIss) {

				if (anIs != null) {

					streams.add(anIs);
				}
			}
			
			currentStream = streams.get(currentIndex);

		} else {

			streams = Collections.emptyList();
			currentStream = null;
		}
	}

	public ArrayInputStream(Collection<InputStream> inIs) {

		if (inIs != null) {

			streams = new ArrayList<InputStream>(inIs.size());

			for (final InputStream anIs : inIs) {

				if (anIs != null) {

					streams.add(anIs);
				}
			}

			currentStream = streams.get(currentIndex);
			
		} else {

			streams = Collections.emptyList();
			currentStream = null;
		}

	}

	@Override
	public int read() throws IOException {

		if (currentStream == null) {

			return -1;
		}
		
		final int r = currentStream.read();

		if (r == -1) {

			currentIndex++;
			
			currentStream = currentIndex >= streams.size() ? null : streams.get(currentIndex);
			
			return read();
		}

		return r;
	}

	@Override
	public int available() throws IOException {

		return currentStream == null ? 0 : currentStream.available();
	}

	@Override
	public void close() throws IOException {

		IOException theIOException = null;

		for (final InputStream theIs : streams) {

			try {

				theIs.close();

			} catch (final IOException e) {

				theIOException = theIOException != null ? theIOException : e;
			}
		}

		if (theIOException != null) {
			throw theIOException;
		}

	}

	@Override
	public synchronized void mark(int readlimit) {

		super.mark(readlimit);
	}

	@Override
	public synchronized void reset() throws IOException {

		throw new UnsupportedOperationException();
	}

	@Override
	public boolean markSupported() {

		throw new UnsupportedOperationException();
	}
}
