package de.greyshine.tools;

import java.io.IOException;

import org.junit.Test;

public class CharacterScannerTest {
	
	@Test
	public void test() throws IOException {
		CharacterScanner.main("src/test/characterscanner/", "160" , "UTF-8" );
	}

}
