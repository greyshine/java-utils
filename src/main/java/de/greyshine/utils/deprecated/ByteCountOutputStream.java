package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.OutputStream;

public class ByteCountOutputStream extends OutputStream {

	private long count = 0;

	private final OutputStream os;

	public ByteCountOutputStream(OutputStream inOutputStream) {
		os = inOutputStream;
	}

	@Override
	public void write(int b) throws IOException {

		count++;
		os.write(b);
	}

	public long getCount() {
		return count;
	}

	@Override
	public String toString() {
		return "ByteCountOutputStream [count=" + count + ", outputstream=" + os + "]";
	}

}
