package de.greyshine.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.UUID;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class JedisWrapperTests {
	
	private JedisWrapper jedisWrapper;

	@Before
	public void before() {
		
		jedisWrapper = new JedisWrapper("192.168.56.101", "6333");
	}
	
	@After
	public void after() {
		
		jedisWrapper.close();

	}
	
	@Ignore
	@Test
	public void test() throws IOException {
		
		jedisWrapper.set( UUID.randomUUID().toString() , ""+ System.currentTimeMillis());
		
		jedisWrapper.keys( null, (inKey) -> {
			
			  System.out.println( inKey );
			
		} , true );
		
		final File file = new File("src/test/resources/test.png");
		final String theMd5 = Utils.getMd5( file );

		System.out.println( "MD5: "+ theMd5 );
		
		final String theId = jedisWrapper.setBinary( new FileInputStream( file ), true );
		System.out.println( "newId: "+ theId );
		
		byte[] data = jedisWrapper.getBinary( theId );
		
		String theMd5_2 = Utils.getMd5( data );
		
		System.out.println( "MD5.2: "+ theMd5_2 );
		
		Utils.write( "target/"+ file.getName(), data);
		
	}
	
	@Ignore
	@Test
	public void testBinary() throws IOException {
		
		final byte[] bytes1 = new byte[] {0,1,2,3};
		String md5_1 = Utils.getMd5( bytes1 );
		
		String id = jedisWrapper.setBinary( bytes1 );
		
		byte[] bytes2 = jedisWrapper.getBinary( id );
		Assert.assertEquals( bytes1.length , bytes2.length);

		String md5_2 = Utils.getMd5( bytes2 );
		
		
		for (int i = 0; i < bytes2.length; i++) {
			System.out.println(bytes2[i]);
		}
		
		Assert.assertEquals( md5_1 , md5_2);
		
	}
	
}
