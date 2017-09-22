package de.greyshine.utils.beta;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.deprecated.Utils;

public class TikaUtilsTests {

	@Test
	public void test() throws Exception {
		
		String theText = TikaUtils.pdfToString( new File("src/test/resources/tika/helloworld.pdf") );

		Assert.assertEquals("Hello World!", theText.trim());
		
		StringBuilder sb = new StringBuilder();
		
		TikaUtils.parsePdf(new File("src/test/resources/tika/copyprotected.pdf"), (idx,line)->{
			sb.append( idx+": "+ line );
		});
		
		final String theMd5 = Utils.getMd5( sb.toString() );
		sb.setLength(0);
		Assert.assertEquals("4fb4078c62b2a0172fb2392e42eb82d5", theMd5);
		
	}
	
	
	
}
