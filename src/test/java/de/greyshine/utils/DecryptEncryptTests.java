package de.greyshine.utils;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class DecryptEncryptTests {

	@Test
	public void testStandard() throws IOException {
		
		String s = "Hallo Welt";
		String password = "SomePAssWrd";
		
		String encrypted = Utils.encrypt( s, password );
		
		System.out.println( encrypted );
		
		String decrypted = Utils.decrypt( encrypted, password );
		
		Assert.assertEquals( s , decrypted );
	}
	
}
