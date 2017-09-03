package de.greyshine.utils.beta.objectfilestorage;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Locale;
import java.util.concurrent.atomic.AtomicLong;

import de.greyshine.utils.beta.objectfilestorage.IObjectStorage.IIdGenerator;

public class DefaultIdGenerator implements IIdGenerator {

	public final static DefaultIdGenerator INSTANCE = new DefaultIdGenerator();
	
	private static final DateTimeFormatter DTF = new DateTimeFormatterBuilder().appendPattern( "yyyy-MM-dd_HH:mm:ss.SSS" ).toFormatter( Locale.getDefault() );
	private static final AtomicLong ids = new AtomicLong(0L); 
	
	@Override
	public String generate() {
		return DTF.format( LocalDateTime.now() )+"-"+ids.getAndIncrement();
	}
}
