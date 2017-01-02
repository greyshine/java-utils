package de.greyshine.utils;

import java.io.File;
import java.io.IOException;
import java.util.Map;

public abstract class Utils {
	
	public static final Class<?>[] EMPTY_CLASSES = new Class[0];
	
	private Utils() {}

	// defaults
	
	public static <T> T defaultIfNull(T inValue, T inDefault) {
		return inValue == null ? inDefault : inValue;
	}
	
	public static <T> T trimToNull(T inValue) {
		return trimToDefault(inValue, null);
	}
	
	public static String trimToBlank(String inValue) {
		return trimToDefault(inValue, "");
	}
	
	public static <T> T trimToDefault(T inValue, T inDefault) {
		return inValue == null ? inDefault : inValue;
	}
	
	public RuntimeException toRuntimeException(Exception e) {
		return e == null || e instanceof RuntimeException ? (RuntimeException)e : new RuntimeException(e);
	}
	
	
	// ------------
	// File related
	// ------------

	public static boolean isFile(File inFile) {
		return inFile != null && inFile.isFile();
	}
	
	public static boolean isDir(File inFile) {
		return inFile != null && inFile.isDirectory();
	}
	
	public static File getCanonicalFile(File inFile) {
		return getCanonicalFile(inFile, false);
	}
	
	public static File getCanonicalFile(File inFile, boolean inThrowRuntimeException) {

		try {
		
			return inFile == null ? null : inFile.getCanonicalFile();
		
		} catch (final IOException e) {

			if (inThrowRuntimeException) {
				throw new RuntimeException(inFile == null ? "File is null" : e.getMessage(), e);
			}

			return inFile.getAbsoluteFile();
		}
	}
	
	/**
	 * 
	 * @param inDir
	 * @param inExceptionMessage
	 */
	public static void assertExistingDir(File inDir, String inExceptionMessage) {
		if (!isDir(inDir) ) {
			throw new IllegalStateException(inExceptionMessage);
		}
	}
	
	//
	// interfaces
	//
	
	/**
	 * Interface indicating to handle an object and re-inform about further processing
	 * 
	 * @param <T>
	 */
	public static interface IHandler<T> {

		/**
		 * 
		 * @param inObject
		 * @param inIdx
		 * @return <code>true</code> when handling should be continued, otherwise <code>false</code>
		 */
		boolean handle(T inObject, int inIdx);

		/**
		 * 
		 * @param inIdx
		 * @param inException
		 * @param inObject
		 * @return <code>true</code> when handling should be continued, otherwise <code>false</code>
		 */
		boolean handleException(int inIdx, Exception inException, T inObject);

		void done(int inHandles);
	}
	
	/**
	 * Handler for handling items of a {@link Map}
	 * 
	 * 
	 * @param <S>
	 * @param <T>
	 */
	public static interface IMapHandler<S, T> {

		boolean handle(S inKey, T inValue);

		boolean handleException(S inKey, Exception inException, T inValue);

		void done(int inHandles);
	}
}
