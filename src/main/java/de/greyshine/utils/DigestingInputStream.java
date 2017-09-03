package de.greyshine.utils;

import java.io.IOException;
import java.io.InputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class DigestingInputStream extends InputStream {

	private final InputStream inputStream;
	private final MessageDigest messageDigest;

	public DigestingInputStream( InputStream inIs, String inDigestAlgorithm ) throws NoSuchAlgorithmException {
		
		inputStream = inIs;
		
		messageDigest = MessageDigest.getInstance(inDigestAlgorithm);
		
	}
	
	@Override
	public int available() throws IOException {
		return inputStream.available();
	}

	@Override
	public int read() throws IOException {
		
		final int r = inputStream.read();
		
		if ( r == Utils.EOF_STREAM ) {
			return r;
		}
		
		messageDigest.update( (byte) r );

		return r;
	}

	@Override
	public void close() throws IOException {
		
		inputStream.close();
	}
	
	
	public String getDigest() {
		
		final byte[] theHashBytes = messageDigest.digest();
		// converting byte array to Hexadecimal String
		final StringBuilder sb = new StringBuilder(2 * theHashBytes.length);
		for (final byte b : theHashBytes) {
			sb.append(String.format("%02x", b & 0xff));
		}

		return sb.toString();
	}
}
