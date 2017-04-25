package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;


public class ReadToTests {
	
	private final byte[] BYTES_1024 = new byte[1024];
	private final byte[] BYTES_333 = new byte[333];
	
	{
		for( int i = 0, l= BYTES_1024.length; i < l; i++ ) {
			BYTES_1024[i] = (byte) Utils.RANDOM.nextInt(255); 
		} 
		for( int i = 0, l= BYTES_333.length; i < l; i++ ) {
			BYTES_333[i] = (byte) Utils.RANDOM.nextInt(255); 
		} 
	}

	@Test
	public void readInputStreamToBytes() throws IOException {
		
		String hw = "Hello World!";
		
		byte[] bytes = Utils.readToBytes( new ByteArrayInputStream( hw.getBytes( Utils.CHARSET_UTF8 ) ) , true);
		
		Assert.assertEquals(hw, new String( bytes, Utils.CHARSET_UTF8 ));
		
		Assert.assertTrue( Utils.isEqualByteArrays(hw.getBytes( Utils.CHARSET_UTF8 ) , bytes, false) );
		
	}

	@Test
	public void readInputStreamToBytes1024() throws IOException {
		
		byte[] bytes = Utils.readToBytes( new ByteArrayInputStream( BYTES_1024 ) , true);
		
		Assert.assertEquals(BYTES_1024.length, bytes.length );
		
		for ( int i= 0; i<BYTES_1024.length; i++ ) {
			Assert.assertEquals( BYTES_1024[i] , bytes[i]);
		}
		
		Assert.assertTrue( Utils.isEqualByteArrays(BYTES_1024 , bytes, false) );
		
	}

	@Test
	public void readInputStreamToBytes333() throws IOException {
		
		byte[] bytes = Utils.readToBytes( new ByteArrayInputStream( BYTES_333 ) , true);
		
		Assert.assertEquals(BYTES_333.length, bytes.length );
		
		for ( int i= 0; i<BYTES_333.length; i++ ) {
			Assert.assertEquals( BYTES_333[i] , bytes[i]);
		}
		
		Assert.assertTrue( Utils.isEqualByteArrays(BYTES_333, bytes, false) );
	}
	
}
