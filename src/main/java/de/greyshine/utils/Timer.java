package de.greyshine.utils;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.util.Date;

public class Timer {

	public static final Timer DEFAULT = new Timer();
	
	private final long real = System.currentTimeMillis();
	private long reference;

	private int progression = 1;
	
	public Timer() {
		
		this( System.currentTimeMillis() );
	}
	
	public Timer(long inReferenceNow) {
		setReferenceNow( inReferenceNow );
	}
	
	

	private void setReferenceNow(long inReferenceNow) {
		reference = inReferenceNow;
	}
	
	public long now() {
		
		final long realTimePassed = System.currentTimeMillis() - real;
		final long timePassedByFactor = realTimePassed * progression;
		return reference + timePassedByFactor;
	}

	public long millis() {
		return now();
	}
	
	public Date getDate() {
		return new Date( now() );
	}

	public Instant getInstant() {
		return Instant.ofEpochMilli( now() );
	}

	public LocalDateTime getLocaDateTime() {
		return LocalDateTime.ofInstant( getInstant(), ZoneId.systemDefault());
	}
}
