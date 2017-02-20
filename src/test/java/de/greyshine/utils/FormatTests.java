package de.greyshine.utils;

import java.time.LocalDateTime;

import org.junit.Assert;
import org.junit.Test;

public class FormatTests {

	@Test
	public void testFormatDataSize() {
		
		String theText = Utils.formatDataSize( 4711L );
		Assert.assertEquals( "4.60 KB" , theText);
		
		theText = Utils.formatDataSize( 1014*1024*17*19L );
		Assert.assertEquals( "319.85 MB" , theText);
	}

	@Test
	public void testFormatTime() {
		
		String theText = Utils.formatTime( 4711L );
		Assert.assertEquals( "4s711ms" , theText);
		
		theText = Utils.formatTime( 1014*1024*17*19L );
		Assert.assertEquals( "3d21h9m42s528ms" , theText);
	}
	
	@Test
	public void testFormatDate() {

		String time = Utils.formatDate("dd.MM.yyyy HH:mm:ss", LocalDateTime.now() );
		Assert.assertTrue( time, Utils.isMatch( time , "[0123][0-9].[0-1][0-9].[0-9]{4} [0-2][0-9]:[0-5][0-9]:[0-5][0-9]") );
	}
}
