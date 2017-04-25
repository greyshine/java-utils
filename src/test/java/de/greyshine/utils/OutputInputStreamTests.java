package de.greyshine.utils;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class OutputInputStreamTests {
	
	private static final byte[] BYTES_333 = new byte[333];
	private static final byte[] BYTES_5 = {1,2,3,4,5};
	
	static {
		for( int i = 0, l= BYTES_333.length; i < l; i++ ) {
			BYTES_333[i] = (byte) Utils.RANDOM.nextInt(255); 
		} 
	}

	@Test
	public void test() throws IOException {
		
		final Utils.OutputInputStreams ois = new Utils.OutputInputStreams();
		
		ois.outputStream.write( BYTES_333 );
		ois.outputStream.close();
		
		System.out.println( ois.getBufferSize() );
		System.out.println( ois.inputStream.available() );
		
		final byte[] theResult = Utils.readToBytes( ois.inputStream, false );
		
		Assert.assertEquals( BYTES_333.length , theResult.length);
		
		ois.inputStream.close();
		
		Assert.assertTrue( Utils.isEqualByteArrays(BYTES_333 , theResult, false) );
		
	}
	
}
