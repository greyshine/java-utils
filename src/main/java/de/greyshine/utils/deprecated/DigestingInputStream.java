package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;

public class DigestingInputStream extends InputStream {

	final InputStream is;
	BigDecimal current = BigDecimal.ONE;
	BigDecimal sum = BigDecimal.ZERO;

	public DigestingInputStream(InputStream is) {

		this.is = is;
	}

	public String getDigest() {

		return Long.toHexString(sum.longValue());
	}

	@Override
	public int available() throws IOException {

		return is.available();
	}

	@Override
	public synchronized void mark(int arg0) {

		throw new UnsupportedOperationException();
	}

	@Override
	public boolean markSupported() {

		return false;
	}

	@Override
	public int read(byte[] arg0, int arg1, int arg2) throws IOException {

		int theReads = 0;

		for (; arg2 > 0 && available() > 0; arg1++, arg2--, theReads++) {

			arg0[arg1] = (byte) read();
		}

		return theReads;
	}

	@Override
	public int read(byte[] arg0) throws IOException {

		return read(arg0, 0, arg0.length);
	}

	@Override
	public synchronized void reset() throws IOException {

		throw new UnsupportedOperationException();
	}

	@Override
	public long skip(long arg0) throws IOException {

		long skips = 0;

		while (arg0 > 0 && available() > 0) {

			arg0--;
			read();
			skips++;
		}

		return skips;
	}

	@Override
	public int read() throws IOException {

		final int i = is.read();

		sum = sum.add(current.multiply(new BigDecimal(i)));
		current = current.intValue() == 1024 ? BigDecimal.ONE : current.add(BigDecimal.ONE);

		return i;
	}

	@Override
	public void close() throws IOException {
		is.close();
	}

	@Override
	protected void finalize() throws Throwable {

		try {

			this.is.close();

		} catch (final Exception e) {
			// swallow
		}
	}
}