package de.greyshine.utils.beta.objectfilestorage;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import de.greyshine.utils.Utils;
import de.greyshine.utils.beta.objectfilestorage.IObjectStorage.IStreamHandler;

public class FileStorage implements IStreamHandler {

	final File baseDir;

	public FileStorage(File inBasepath) throws IOException {

		baseDir = (inBasepath != null ? inBasepath : new File(".")).getCanonicalFile();
		
		if ( !baseDir.isDirectory() ) {
			baseDir.mkdirs();
		}

		if (!baseDir.exists() || !baseDir.isDirectory()) {

			throw new IOException("basepath is not accessible: " + inBasepath);
		}
	}

	@Override
	public long write(String inId, InputStream inIs, boolean inClose) throws IOException {
		
		if (inIs == null) {
			return -1;
		}
		
		// create id if not existing
		inId = inId != null ? inId : UUID.randomUUID().toString();

		final File theFile = getCanonicalPathLocation(inId);
		
		final boolean isUpdate = Utils.isFile( theFile );
		
		if (!isUpdate) {
			theFile.getParentFile().mkdirs();
		}
		

		final FileOutputStream fos = new FileOutputStream(theFile);

		final long theSize = Utils.copy(inIs, fos, true, true);

		final Metainfo theMetainfo = !isUpdate ? new Metainfo( theFile ) : Metainfo.loadForFile( theFile ); 
		
		theMetainfo.resetModified();
		
		
		return theSize;
	}

	@Override
	public InputStream read(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation( inId );
		
		if ( f == null || !f.isFile() ) { return null; }
		
		return new FileInputStream( f );
	}

	@Override
	public void delete(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || !f.isFile() ) { return; }
		
		Files.delete( f.toPath() );
	}

	@Override
	public long getSize(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || !f.isFile() ) { return -1; }
		
		return f.length();
	}

	@Override
	public String getSha256(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || !f.isFile() ) { return null; }
		
		try (FileInputStream fis = new FileInputStream(f)) {

			return getDigest("SHA-256", fis);
		} catch (NoSuchAlgorithmException e) {
			throw new IOException(e);
		}
	}

	@Override
	public String getMd5(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || !f.isFile() ) { return null; }
		
		try (FileInputStream fis = new FileInputStream(f)) {

			return getDigest("MD5", fis);
			
		} catch (NoSuchAlgorithmException e) {
			throw new IOException(e);
		}
	}

	@Override
	public LocalDateTime getCreated(String inId) throws IOException {

		final File f = getCanonicalPathLocation(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {

			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.creationTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());

		} catch (IOException e) {

			throw new RuntimeException(e);
		}
	}

	@Override
	public LocalDateTime getUpdated(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {

			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.lastModifiedTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());

		} catch (IOException e) {

			throw new RuntimeException(e);
		}
	}

	@Override
	public LocalDateTime getLastAccessed(String inId) throws IOException {
		
		final File f = getCanonicalPathLocation(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {
			
			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.lastAccessTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());
			
		} catch (IOException e) {
			
			throw new RuntimeException(e);
		}
	}

	public File getCanonicalPathLocation(String inId) throws IOException {
		
		if ( inId == null || inId.trim().isEmpty() ) { return null; }

		try {
		
			final File theFile = new File( baseDir, inId ).getCanonicalFile();
			
			final String theBaseDir = baseDir.getCanonicalPath();
			final String theCheckFile = theFile.getCanonicalPath();

			if ( theCheckFile.startsWith(theBaseDir) ) {
				
				return theFile;
			}
			
			throw new IOException("illegal file access: "+ theFile.getAbsolutePath());
			
		} catch (Exception e) {
			
			throw e instanceof IOException ? (IOException)e : new IOException(e);
		}
	}
	
	private static String getDigest(String inAlgorithm, InputStream inIs) throws IOException, NoSuchAlgorithmException {

		final byte[] bytes = new byte[1024 * 4];

		final MessageDigest md = MessageDigest.getInstance(inAlgorithm);
		while (inIs.available() > 0) {

			final int r = inIs.read(bytes);

			for (int i = 0; i < r; i++) {

				md.update(bytes[i]);
			}
		}

		final byte[] theHashBytes = md.digest();
		// converting byte array to Hexadecimal String
		final StringBuilder sb = new StringBuilder(2 * theHashBytes.length);
		for (final byte b : theHashBytes) {
			sb.append(String.format("%02x", b & 0xff));
		}

		return sb.toString();
	}

	public File getBaseDir() {
		return baseDir;
	}

	public List<String> listIds(String inPrefix) {
		
		final List<String> theIds = new ArrayList<>();
		final boolean isBlankPrefix = Utils.isBlank( inPrefix );
		final int theBasedirLength = baseDir.getAbsolutePath().length()+1;
		
		
		final List<File> q = new ArrayList<>();
		q.add( baseDir );
		
		while( !q.isEmpty() ) {
			
			final File[] theFiles = q.remove(0).listFiles();
			
			for(File aFile : theFiles == null ? Utils.EMPTY_FILES : theFiles) {
				
				if ( aFile.isDirectory() ) {
					
					q.add( aFile );

				} else {
					
					final String theFilePath = aFile.getAbsolutePath().substring( theBasedirLength );

					if ( !isBlankPrefix && !theFilePath.startsWith( inPrefix ) ) {

						continue;
					}
					
					theIds.add( aFile.getAbsolutePath().substring( theBasedirLength ) );
				}
			}
		}
		
		return theIds;
	}


}
