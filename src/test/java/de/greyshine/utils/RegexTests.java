package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;

public class RegexTests {
	
	@Test
	public void test() {
		
		String text = "xyz";
		String regex = "xyz";
		
		Assert.assertTrue( text.matches( regex ) );
		Assert.assertTrue( Utils.isMatch( text , regex) );
		
		regex = "[a-z]*";
		Assert.assertTrue( Utils.isMatch( text , regex) );
	}
	
	@Test
	public void testDatetimeIso() {
		
		String date = "2011-12-03T10:15:59";
		
		Assert.assertTrue( date.matches( Utils.REGEX_DATETIME_ISO ) );
		
	}

}
