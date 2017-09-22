package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;



public class Base64Tests {
	
	@Test
	public void testStream() throws IOException {
		
		String s = "Hello World!";
		String b = Utils.toBase64( s.getBytes( "UTF-8" ) );
		
		Assert.assertEquals( "SGVsbG8gV29ybGQh" , b);
		
		b = Utils.toBase64( new ByteArrayInputStream( s.getBytes("UTF-8") ) );
		Assert.assertEquals( "SGVsbG8gV29ybGQh" , b);
		
		byte[] theBytes = Utils.toBytesFromBase64( b );
		Assert.assertEquals( s, new String( theBytes, Utils.CHARSET_UTF8 ) );
		
	}
	
	@Test
	public void testBytes1() {
		
		final byte[] bytes1 = new byte[] {0,1,2,3,4,5,6,7,8,9,10};
		String md5_1 = Utils.getMd5( bytes1 );
		
		String b64 = Utils.toBase64( bytes1 );
		
		byte[] bytes2 = Utils.toBytesFromBase64(b64);
		String md5_2 = Utils.getMd5( bytes2 );
		
		for (int i = 0; i < bytes2.length; i++) {
			Assert.assertEquals( bytes1[i] , bytes2[i]);
		}
		
		Assert.assertEquals( md5_1 , md5_2);
	}

	@Test
	public void testBytes2() throws IOException {
		
		final byte[] bytes1 = Utils.toBytes( new File("src/test/resources/test.png"));
		
		String md5_1 = Utils.getMd5( bytes1 );
		String b64 = Utils.toBase64( bytes1 );
		
		byte[] bytes2 = Utils.toBytesFromBase64(b64);
		String md5_2 = Utils.getMd5( bytes2 );
		
		Assert.assertEquals( bytes1.length , bytes2.length);
		for (int i = 0; i < bytes2.length; i++) {
			Assert.assertEquals( bytes1[i] , bytes2[i]);
		}

		Assert.assertEquals( md5_1 , md5_2);
		
	}
	
	@Test
	public void test4() throws IOException {
		
		byte[] bytes = {0,1,2,3}; 
		
		String b64_1 = Utils.toBase64( bytes );
		String b64_2 = Utils.toBase64( new ByteArrayInputStream( bytes ) );
		
		Assert.assertEquals( "AAECAw==" , b64_1);
		Assert.assertEquals( b64_1 , b64_2);
		
		byte[] bytes2 = Utils.toBytesFromBase64( b64_1 );
		Assert.assertEquals( bytes.length , bytes2.length);
		
		for (int i = 0; i < bytes2.length; i++) {
			Assert.assertEquals( bytes[i] , bytes[i]);
		}

		
	}

}
