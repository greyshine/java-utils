package de.greyshine.utils;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;


public class FileTests {
	
	@Test
	public void testCanonicalPathLocation() {
		
		Assert.assertFalse( Utils.isCanonicalPathLocation( null, null ) );
		
		final File basepath = new File("target/somewhere");
		Assert.assertFalse( Utils.isCanonicalPathLocation( basepath, null ) );
		
		File theCheckFile = new File("target/somewhere");
		Assert.assertTrue( Utils.isCanonicalPathLocation( basepath, theCheckFile ) );

		theCheckFile = new File("target/somewhere/works");
		Assert.assertTrue( Utils.isCanonicalPathLocation( basepath, theCheckFile ) );

		theCheckFile = new File("target/nowhere/works");
		Assert.assertFalse( Utils.isCanonicalPathLocation( basepath, theCheckFile ) );

		theCheckFile = new File("target/somewhere/../nowhere/works");
		Assert.assertFalse( Utils.isCanonicalPathLocation( basepath, theCheckFile ) );
	}

}
