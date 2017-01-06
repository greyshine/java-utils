package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;

public class ReplacementTests {
	
	@Test
	public void replaceCharactersWithDiacritics() {
		
		String s1 = "üöäÜÖÄßéèâôÔ";
		String s2 = Utils.replaceCharactersWithDiacritics( s1 );
		
		Assert.assertEquals( "ueoeaeUeOeAesseeaoO" , s2);
	}
	
	

}
