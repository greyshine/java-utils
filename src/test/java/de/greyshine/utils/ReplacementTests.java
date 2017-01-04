package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;

public class ReplacementTests {
	
	@Test
	public void replaceCharactersWithDiacritics() {
		
		String s1 = "üöäÜÖÄßé";
		String s2 = Utils.replaceCharactersWithDiacritics( s1 );
		
		Assert.assertEquals( "ueoeaeUeOeAesse" , s2);
	}
	
	

}
