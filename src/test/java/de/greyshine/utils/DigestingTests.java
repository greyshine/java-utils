package de.greyshine.utils;

import org.junit.Test;

public class DigestingTests {
	
	@Test
	public void test256 () {
		
		String s1 = "Hallo Welt!";
		String s2 = Utils.getSha256(s1);
		System.out.println( s2 );
		System.out.println( Utils.getSha1(s1) );
		
	}

}
