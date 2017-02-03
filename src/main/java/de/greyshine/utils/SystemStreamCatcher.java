package de.greyshine.utils;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;


/**
 * Redirecting of the default output and/or error stream.<br/>
 * E.g. useful for catching <tt>System.out</tt> output for test purposes.<br/>
 * <br/>
 * when instantiated, it will redirect <code>{@link System}.out</code> (and <code>{@link System}.err</code>).<br/>
 * Calling <code>end()</code> will stop that behavior and switch back to the stream behviour before.<br/>
 * <br/>
 * Unpredictable behaviour will occur if any other code resets System.out and/or System.err streams.
 * 
 * 
 */
public class SystemStreamCatcher {
	
	private static SystemStreamCatcher activeInstance = null;
	
	private PrintStream out = System.out;
	private PrintStream err = System.err;
	
	private final ByteArrayOutputStream bufferOut = new ByteArrayOutputStream();
	private final ByteArrayOutputStream bufferErr = new ByteArrayOutputStream();
	private PrintStream printSreamOut;
	private PrintStream printSreamErr;
	
	/**
	 * @param inCatchError catch error stream
	 */
	public SystemStreamCatcher(boolean inCatchError) {
		reset( inCatchError );
	}
	
	public SystemStreamCatcher reset(boolean inCatchError) {
		
		synchronized ( SystemStreamCatcher.class ) {
		
			if ( activeInstance != null ) {
				throw new IllegalStateException( "Call end() on active instance first [instance="+ activeInstance +"]" );
			}
			activeInstance = this;
		}
		
		bufferOut.reset();
		bufferErr.reset();
		
		System.setOut( printSreamOut = new PrintStream( bufferOut ) {
			@Override
			public void close() {
				super.flush();
			}
		} );
		
		if ( inCatchError ) {
			System.setErr( printSreamErr = new PrintStream( bufferErr ) {
				@Override
				public void close() {
					// swallow
					super.flush();
				}
			} );
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
		return activeInstance == this;
	}
	
	public boolean isOutputStreamCorrupted() {
		return printSreamOut != null && System.out != printSreamOut;
	}
	public boolean isErrorStreamCorrupted() {
		return printSreamErr != null && System.err != printSreamErr;
	}
	
	public SystemStreamCatcher end() {
		
		synchronized ( SystemStreamCatcher.class ) {
			
			if ( isOutputStreamCorrupted() ) {
				throw new IllegalStateException( "System.out corrupted. System.out changed [System.out="+ System.out +"]" );
			}
			
			System.setOut( out );
			printSreamOut.close();
			printSreamOut = null;
			
			if ( isErrorStreamCorrupted() ) {
				throw new IllegalStateException( "System.err corrupted. System.out changed [System.err="+ System.err +"]" );
			}
			
			System.setErr( err );
			if ( printSreamErr != null ) {
				printSreamErr.close();
				printSreamErr = null;
			}
			
			activeInstance = null;
		}
		
		return this;
	}
}
