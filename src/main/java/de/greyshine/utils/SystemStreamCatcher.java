package de.greyshine.utils;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

public class SystemStreamCatcher {
	
	private PrintStream out = System.out;
	private PrintStream err = System.err;
	
	private final ByteArrayOutputStream bufferOut = new ByteArrayOutputStream();
	private final ByteArrayOutputStream bufferErr = new ByteArrayOutputStream();
	
	public SystemStreamCatcher(boolean inCatchError) {
		reset( inCatchError );
	}
	
	public SystemStreamCatcher reset(boolean inCatchError) {
		
		bufferOut.reset();
		bufferErr.reset();
		
		System.setOut( new PrintStream( bufferOut ) );
		
		if ( inCatchError ) {
			System.setErr( new PrintStream( bufferErr ) );
		} else {
			System.setErr( err );
		}
		
		return this;
	}

	public byte[] getDataSystemOut() {
		return bufferOut == null ? null : bufferOut.toByteArray();
	}
	public String getDataSystemOutAsString() {
		return bufferOut == null ? null : new String( bufferOut.toByteArray() );
	}
	
	public byte[] getDataSystemErr() {
		return bufferErr == null ? null : bufferErr.toByteArray();
	}
	
	public String getDataSystemErrAsString() {
		return bufferErr == null ? null : new String( bufferErr.toByteArray() );
	}
	
	public boolean isActive() {
		return out != null || err != null;
	}
	
	public SystemStreamCatcher end() {
		System.setOut( out );
		System.setErr( err );
		return this;
	}
}
