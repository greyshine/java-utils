package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.ShowdownTransformer;

public class ShowdownTransformerTest {
	
	@Test
	public void test() throws Exception {
		
		String in = "# Hello\n## World\n";
		String out = ShowdownTransformer.toHtml( in );

		Assert.assertEquals( "<h1id=\"hello\">Hello</h1><h2id=\"world\">World</h2>" , out.replaceAll( "\\s" , ""));
		
		Assert.assertEquals( "<h1 id=\"merhaba\">Merhaba</h1>" , Utils.markdownToHtml( "#Merhaba" ) );
	}
}
