package de.greyshine.utils;

import java.time.LocalDateTime;
import java.time.ZoneId;

import org.junit.Assert;
import org.junit.Test;


public class DateTests {

	@Test
	public void testMillisToLocalDateTime() {
		
		final long theMillis = 1500661296814L;
		final String theGmtTime = "2017-07-21T18:21:36.814";
		
		final long theTime = System.currentTimeMillis();
		
		final LocalDateTime theLocalDateTime = Utils.millisToLocalDateTime( theMillis, ZoneId.of( "GMT" ) );
		Assert.assertEquals( theGmtTime , theLocalDateTime.toString());
	}
	
}
