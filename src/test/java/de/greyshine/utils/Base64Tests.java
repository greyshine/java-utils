package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.UnsupportedEncodingException;

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
	}
}
