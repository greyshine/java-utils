package de.greyshine.utils.beta.objectfilestorage;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import de.greyshine.utils.Timer;
import de.greyshine.utils.Utils;
import de.greyshine.utils.beta.JsonPersister;

public class Metainfo {
	
	private static final JsonPersister JSON_PERSISTER = new JsonPersister();
	
	static final String DTF = "yyyy-mm-dd HH:mm:ss:SSS";
	
	private File file;

	/**
	 * do not rely on the os's file system stamps since the file system could have been copied in the meantime
	 */
	public LocalDateTime created = Timer.DEFAULT.getLocaDateTime();
	public LocalDateTime modified = created;
	
	public String md5;
	public String sha256;
	
	final List<String> tags = new ArrayList<>(0);

	
	public Metainfo(File inFile) {
	
		if ( Utils.isNoFile( inFile ) ) {
			throw new IllegalArgumentException( inFile +" is no readable file" );
		}

		file = inFile;
		
		try {
		
			final BasicFileAttributes attr = Files.readAttributes(inFile.toPath(), BasicFileAttributes.class);
			attr.creationTime().toInstant();
			created = Utils.millisToLocalDateTime( attr.creationTime().toMillis() );
			modified = Utils.millisToLocalDateTime( attr.creationTime().toMillis() );
		
		} catch (IOException e) {
			throw Utils.toRuntimeException(e);
		}
		
		md5 = Utils.getMd5( inFile );
		sha256 = Utils.getSha256( inFile );
	}

	public static Metainfo loadForFile(File inFile) throws IOException {
		
		final File theInfoFile = new File( inFile.getParentFile(), inFile.getName()+".info" );
		
		return JSON_PERSISTER.read(theInfoFile, Metainfo.class);
	}
	
	public void write() throws IOException {
		
		final File theInfoFile = new File( file.getParentFile(), file.getName()+".info" );
		JSON_PERSISTER.save(theInfoFile, this);
	}

	public void resetModified() {
		
		modified = Timer.DEFAULT.getLocaDateTime();
		
		md5 = Utils.getMd5( file );
		sha256 = Utils.getSha256( file );
	}
	
}
