package de.greyshine.tools;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.SystemStreamCatcher;

public class CharacterScannerTest {
	
	@Test
	public void testNoCharArg() throws IOException {
		
		final SystemStreamCatcher ssc = new SystemStreamCatcher(false);
		
		CharacterScanner.main("-charset","UTF-8", "src/test/characterscanner/" );
		
		ssc.end();
		
		final String theOutput = ssc.getDataSystemOutAsString(); 

		System.out.println( theOutput );
		
		Assert.assertTrue( theOutput.contains( File.separatorChar + "file1" ) );
		
		Assert.assertTrue( theOutput.contains( "0 : 1 : 97 = a" ) );
		Assert.assertTrue( theOutput.contains( "0 : 9 : 116 = t" ) );
		
		Assert.assertTrue( theOutput.contains( File.separatorChar + "file2" ) );
		
		Assert.assertTrue( theOutput.contains( "0 : 0 : 72 = H" ) );
		Assert.assertTrue( theOutput.contains( "0 : 11 : 100 = d" ) );
		Assert.assertTrue( theOutput.contains( "1 : 0 : 100 = d" ) );
		Assert.assertTrue( theOutput.contains( "1 : 5 : 39 = '" ) );
	}

	@Test
	public void test() throws IOException {
		
		final SystemStreamCatcher ssc = new SystemStreamCatcher(false);
		
		CharacterScanner.main("-charset","UTF-8", "-charnum", String.valueOf( (int)'e' ), "src/test/characterscanner/" );
		ssc.end();
		
		String theOutput = ssc.getDataSystemOutAsString(); 
		
		System.out.println( theOutput );
		
		Assert.assertTrue( theOutput.contains( "file1.txt: 7, 0, 7" ) );
		Assert.assertTrue( theOutput.contains( "file2.txt: 2, 0, 2" ) );
		
		ssc.reset( false );
		CharacterScanner.main("-charset","UTF-8", "-charnum", String.valueOf( (int)'\'' ), "src/test/characterscanner/" );
		ssc.end();
		
		theOutput = ssc.getDataSystemOutAsString();
		System.out.println( theOutput );
		Assert.assertTrue( theOutput.contains( "file2.txt: 5, 0, 5" ) );
		Assert.assertTrue( theOutput.contains( "file2.txt: 7, 0, 7" ) );
		Assert.assertTrue( theOutput.contains( "file2.txt: 15, 1, 3" ) );
		Assert.assertTrue( theOutput.contains( "file2.txt: 17, 1, 5" ) );
		
		
	}
}
