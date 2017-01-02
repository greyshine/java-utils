package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.InputStream;

public class ByteCountInputStream extends InputStream {

	private long count;
	private final InputStream inputStream;

	public ByteCountInputStream(InputStream inInputStream) {

		this(inInputStream, 0L);
	}

	public ByteCountInputStream(InputStream inInputStream, long inStartCount) {

		count = inStartCount;
		inputStream = inInputStream;
	}

	public final long getCount() {

		return count;
	}

	public void notifyBytesAdded(long inCount) throws IOException {

	}

	@Override
	public int read() throws IOException {

		final int theByte = inputStream.read();

		count++;

		notifyBytesAdded(count);

		return theByte;
	}

	@Override
	public int read(byte[] b, int off, int len) throws IOException {

		final int read = super.read(b, off, len);

		count += read;

		notifyBytesAdded(count);

		return read;
	}

	@Override
	public int read(byte[] b) throws IOException {

		final int read = super.read(b);

 		count += read;

		notifyBytesAdded(count);

		return read;
	}
	
	@Override
	public void close() throws IOException {
		
		inputStream.close();
	}

	@Override
	public String toString() {
		return "ByteCountInputStream [count=" + getCount() + ", inputStream="
				+ inputStream + "]";
	}
}
