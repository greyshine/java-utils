package de.greyshine.utils.beta.objectfilestorage;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.time.LocalDateTime;
import java.util.List;
import java.util.UUID;
import java.util.function.Function;

public interface IObjectStorage extends Closeable {

	String save( Object inObject ) throws IOException;
	<T> T read(Class<T> inType, String id ) throws IOException;
	List<Class<?>> listTypes();
	long countEntries();
	long countEntries(Class<?> inType);
	long getStorageSize();
	long getStorageSize(Class<?> inType);
	<T> List<T> filter(Class<T> inClass, Function<T, Boolean> inFilter );
	
	/**
	 * Generator for generating ids
	 * @author greyshine
	 *
	 */
	interface IIdGenerator {
		
		default String generate() {
			
			return UUID.randomUUID().toString();
		}
	}
	
	interface IStreamHandler {
		long write(String inId, InputStream inIs, boolean inClose) throws IOException;
		InputStream read(String inId) throws IOException;
		void delete(String inId) throws IOException;
		long getSize(String inId) throws IOException;
		String getSha256(String inId) throws IOException;
		String getMd5(String inId) throws IOException;
		LocalDateTime getCreated(String inId) throws IOException;
		LocalDateTime getUpdated(String inId) throws IOException;
		LocalDateTime getLastAccessed(String inId) throws IOException;
	}
}
