package de.greyshine.utils;

import org.junit.Test;

import org.junit.*;

public class StreamTests {
	
	@Test
	public void arrayToStream() {
		
		final StringBuilder sb = new StringBuilder();
		
		Utils.toStream( "A", "B", "C" ).forEach( s->{ sb.append( s ); } );
		
		Assert.assertEquals( "ABC" , sb.toString());
	}
	
	

}
