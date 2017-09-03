package de.greyshine.utils.beta;

import java.io.File;

import org.junit.Test;

public class TikaUtilsTests {

	@Test
	public void test() throws Exception {
		
		String theText = TikaUtils.pdfToString( new File("src/test/resources/tika/helloworld.pdf") );
		//theText = TikaUtils.pdfToString( new File("src/test/resources/tika/copyprotected.pdf") );
		
		TikaUtils.parsePdf(new File("src/test/resources/tika/copyprotected.pdf"), (idx,line)->{
			
			System.out.println( idx+": "+ line );
			
		});
		
		
		System.out.println("XXX: "+  theText );

	}
	
	
	
}
