package de.greyshine.tools;

import java.io.File;
import java.nio.charset.Charset;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.Utils;

public class AlphabetStreamerTest {
	
	@Test
	public void testSimple() throws Exception {
		
		final File out1 = new File("target/alphabetstreamer/out1.txt");
		out1.delete();
		Assert.assertFalse( Utils.isFile( out1 ) );
		final File out2 = new File("target/alphabetstreamer/out2.txt");
		out2.delete();
		Assert.assertFalse( Utils.isFile( out2 ) );
		
		String[] theArgs = new String[] {
				"-e",
				"-f","src/test/alphabetstreamer/input.txt",
				"-o", out1.getAbsolutePath()
		};
		
		AlphabetStreamer.main( theArgs );
		
		theArgs = new String[] {
				"-d",
				"-f", out1.getAbsolutePath(),
				"-o", out2.getAbsolutePath()
		};
		
		AlphabetStreamer.main( theArgs );
		
		Assert.assertEquals( "Hallo Welt! * Hello World! * Allo la monde!" , Utils.readToString( out2, Charset.defaultCharset() ));
	}
	
	@Test
	public void testPassword() throws Exception {
		
		final File out3 = new File("target/alphabetstreamer/out3.txt");
		out3.delete();
		Assert.assertFalse( Utils.isFile( out3 ) );
		final File out4 = new File("target/alphabetstreamer/out4.txt");
		out4.delete();
		Assert.assertFalse( Utils.isFile( out4 ) );
		
		String[] theArgs = new String[] {
				"-e",
				"-f","src/test/alphabetstreamer/input.txt",
				"-o", out3.getAbsolutePath(),
				"-p", "passwort"
		};
		
		AlphabetStreamer.main( theArgs );
		
		theArgs = new String[] {
				"-d",
				"-f", out3.getAbsolutePath(),
				"-o", out4.getAbsolutePath(),
				"-p", "passwort"
		};
		
		AlphabetStreamer.main( theArgs );
		
		Assert.assertEquals( "Hallo Welt! * Hello World! * Allo la monde!" , Utils.readToString( out4, Charset.defaultCharset() ));
	}

}
