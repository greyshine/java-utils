package de.greyshine.utils;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;

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
	
	@Test
	public void testParseLocalDateTime() {
		Assert.assertEquals("2017-11-23T23:21:12", Utils.parseLocalDateTime("yyyy-MM-dd'T'HH:mm:ss", "2017-11-23T23:21:12").format( DateTimeFormatter.ISO_LOCAL_DATE_TIME ));
		Assert.assertEquals("2017-11-11T23:21:00", Utils.parseLocalDateTime("dd.MM.yy HH:mm", "11.11.17 23:21").format( DateTimeFormatter.ISO_LOCAL_DATE_TIME ));
	}

	@Test
	public void testParseLocalDate() {
		Assert.assertEquals("2017-11-23", Utils.parseLocalDate("yyyy-MM-dd'T'HH:mm:ss", "2017-11-23T23:21:12").format( DateTimeFormatter.ISO_LOCAL_DATE ));
		Assert.assertEquals("1977-08-02", Utils.parseLocalDate("dd.MM.yyyy", "02.08.1977").format( DateTimeFormatter.ISO_LOCAL_DATE ));
	}
	
}
