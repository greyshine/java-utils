package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;


public class StringTests {

	@Test
	public void unwrap() {
		String s = "'Hello'";
		Assert.assertEquals( "Hello" , Utils.unwrap(s, '\''));
		s = "\"Hello\"";
		Assert.assertEquals( "Hello" , Utils.unwrap(s, '\"'));
		s = "Hello'";
		Assert.assertEquals( "Hello'" , Utils.unwrap(s, '\"'));
		s = "' Hello'";
		Assert.assertEquals( " Hello" , Utils.unwrap(s, '\''));
		s = " ' Hello'";
		Assert.assertEquals( " ' Hello'" , Utils.unwrap(s, '\''));
	}
	
}
