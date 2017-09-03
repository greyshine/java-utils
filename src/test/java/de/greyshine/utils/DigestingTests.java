package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;

import org.junit.Assert;
import org.junit.Test;


public class DigestingTests {
	
	final static String helloWorld = "Hallo Welt!";
	
	@Test
	public void testSha1 () {
		Assert.assertEquals( "726c3e8861ab0652a5043ea5faff6d3ef33fb209" ,  Utils.getSha1( helloWorld ) );
	}
	
	@Test
	public void testSha256 () {
		String s2 = Utils.getSha256(helloWorld);
		Assert.assertEquals( "a582e8c28249fe7d7990bfa0afebd2da9185a9f831d4215b4efec74f355b301a" , s2);
	}

	@Test
	public void testMd5 () {
		Assert.assertEquals( "55243ecf175013cfe9890023f9fd9037" , Utils.getMd5(helloWorld));
	}
	
	@Test
	public void testStreamMd5() throws NoSuchAlgorithmException, IOException {
		
		final DigestingInputStream dis = new DigestingInputStream( new ByteArrayInputStream( helloWorld.getBytes() ) , "MD5");
		
		Utils.copy( dis , Utils.DEV0);
		
		dis.close();
		
		Assert.assertEquals( "55243ecf175013cfe9890023f9fd9037" , dis.getDigest());
	}

	@Test
	public void testStreamSha256() throws NoSuchAlgorithmException, IOException {
		
		final DigestingInputStream dis = new DigestingInputStream( new ByteArrayInputStream( helloWorld.getBytes() ) , "sha-256");
		
		Utils.copy( dis , Utils.DEV0);
		
		dis.close();
		
		Assert.assertEquals( "a582e8c28249fe7d7990bfa0afebd2da9185a9f831d4215b4efec74f355b301a" , dis.getDigest());
	}

}
