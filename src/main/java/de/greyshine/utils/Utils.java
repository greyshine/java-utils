package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;
import java.util.Set;
import java.util.regex.Pattern;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import de.greyshine.utils.deprecated.ReflectionUtils;


public abstract class Utils {
	
	public static final Class<?>[] EMPTY_CLASSES = new Class<?>[0];
	public static final Object[] EMPTY_OBJECTS = new Object[0];
	public static final String[] EMPTY_STRINGS = new String[0];
	public static final byte[] EMPTY_BYTES = new byte[0];
	public static final File[] EMPTY_FILES = new File[0];
	public static final InputStream EMPTY_INPUTSTREAM = new ByteArrayInputStream(new byte[0]);
	
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static final Set EMPTY_SET = Collections.unmodifiableSet(new HashSet());
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static final List EMPTY_LIST = Collections.unmodifiableList(new ArrayList());
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static final Map EMPTY_MAP = Collections.unmodifiableMap(new HashMap());

	public static final Random RANDOM = new Random();

	public static final Charset CHARSET_UTF8 = Charset.forName("UTF-8");
	public static final Charset CHARSET_ISO_8859_1 = Charset.forName("ISO-8859-1");

	public static final BigDecimal BD_1024 = new BigDecimal("1024");
	public static final BigDecimal BD_360 = new BigDecimal("360");
	public static final BigDecimal BD_180 = new BigDecimal("180");
	public static final BigDecimal BD_100 = new BigDecimal("100");
	public static final BigDecimal BD_90 = new BigDecimal("90");
	public static final BigDecimal BD_m360 = BD_360.negate();
	public static final BigDecimal BD_m180 = BD_180.negate();
	public static final BigDecimal BD_m90 = BD_90.negate();

	public static final BigDecimal BD_FLOAT_MAX = new BigDecimal(Float.MAX_VALUE);
	public static final BigDecimal BD_FLOAT_MIN = new BigDecimal(Float.MIN_VALUE);
	public static final BigDecimal BD_DOUBLE_MAX = new BigDecimal(Double.MAX_VALUE);
	public static final BigDecimal BD_DOUBLE_MIN = new BigDecimal(Double.MIN_VALUE);

	public static final long MILLIS_1_SECOND = 1000L;
	public static final long MILLIS_1_MINUTE = MILLIS_1_SECOND * 60L;
	public static final long MILLIS_1_HOUR = MILLIS_1_MINUTE * 60L;
	public static final long MILLIS_1_DAY = MILLIS_1_HOUR * 24L;

	public static final long BYTES_1KB = 1024;
	public static final long BYTES_1MB = BYTES_1KB * 1024;
	public static final long BYTES_1GB = BYTES_1MB * 1024;
	public static final long BYTES_1TB = BYTES_1GB * 1024;
	public static final long KILOBYTE_1MB = 1024;
	public static final long KILOBYTE_1GB = KILOBYTE_1MB * 1024;
	public static final long KILOBYTE_1TB = KILOBYTE_1GB * 1024;
	public static final long MEGABYTE_1GB = 1024;
	public static final long MEGABYTE_1TB = MEGABYTE_1GB * 1024;
	
	public static final String ALPHABET_0to9 = "0123456789";
	public static final String ALPHABET_HEX = "0123456789abcdef";
	public static final String ALPHABET_SMALL = "0123456789abcdefghijklmnopqrstuvwxyz";
	public static final String ALPHABET_LARGE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	
	public static final Pattern DIACRITICS_AND_FRIENDS = Pattern.compile("[\\p{InCombiningDiacriticalMarks}\\p{IsLm}\\p{IsSk}]+");
	
	public static Map<String,String> UMLAUT_REPLACEMENTS = new MapBuilder<String,String>()//
			.put("\u00E4", "ae")//
			.put("\u00F6", "oe")//
			.put("\u00FC", "ue")//
			.put("\u00C4", "Ae")//
			.put("\u00D6", "Oe")//
			.put("\u00DC", "Ue")//
			.put("\u00DF", "ss")//
			.get(true);
	
	/**
	 * http://stackoverflow.com/questions/4448829/regular-expression-for-not-
	 * empty
	 */
	public final static String REGEX_NOT_EMPTY = ".*\\S.*";
	public final static Pattern PATTERN_NOT_EMPTY = Pattern.compile( REGEX_NOT_EMPTY );
	
	public static final Comparator<File> FILE_COMPARATOR = new Comparator<File>() {

		@Override
		public int compare(File f1, File f2) {

			if (f1.isDirectory() && f2.isFile()) {

				return 1;

			} else if (f1.isFile() && f2.isDirectory()) {

				return -1;
			}

			return f1.getName().compareTo(f2.getName());
		}
	};
	
	@SuppressWarnings("rawtypes")
	public static final Iterator ITERATOR_NONE = new Iterator() {

		@Override
		public boolean hasNext() {
			return false;
		}

		@Override
		public Object next() {
			throw new IllegalStateException("no more elements");
		}
	};
	
	@SuppressWarnings("rawtypes")
	public static final Iterable ITERABLE_NONE = new Iterable() {
		@Override
		public Iterator iterator() {
			return ITERATOR_NONE;
		}
	};
	
	private Utils() {}

	// defaults
	
	public static boolean equals(Object inO1, Object inO2) {

		return equals(inO1, inO2, true);
	}
	
	public static boolean equals(Object inO1, Object inO2, boolean isBothNullEqual) {

		if (inO1 == null && inO2 == null) {
			return isBothNullEqual;
		} else if (inO1 == null && inO2 != null) {
			return false;
		} else if (inO2 == null && inO1 != null) {
			return false;
		}

		return inO1.equals(inO2);
	}

	public static boolean notEquals(Object inO1, Object inO2) {

		return !equals(inO1, inO2, true);
	}
	
	public static <T> T defaultIfNull(T inValue, T inDefault) {
		return inValue == null ? inDefault : inValue;
	}
	
	public static String defaultIfBlank(String inValue, String inDefault) {
		return inValue == null || inValue.trim().isEmpty() ? inDefault : inValue;
	}
	
	public static String nullIfBlank(String inValue, String inDefault) {
		return inValue == null || inValue.trim().isEmpty() ? null : inValue;
	}
	
	public static String emptyIfBlank(String inValue, String inDefault) {
		return inValue == null || inValue.trim().isEmpty() ? "" : inValue;
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
	
	public static <T> T requireNonNull(T inValue) {
		return requireNonNull(inValue, null);
	}
	public static <T> T requireNonNull(T inValue, String inMessage) {
		
		inMessage = defaultIfBlank( inMessage, "Value must have not have been null." );
		
		if ( inValue == null ) {
			throw new IllegalArgumentException( inMessage );
		}
		
		return inValue;
	}
	
	public static <T> Wrapper<T> wrapper(T inValue) {
		return new Wrapper<T>(inValue);
	}
	
	public static <T> Wrapper<T> wrapper() {
		return new Wrapper<T>();
	}
	
	public static boolean isTrue(Boolean inBoolean) {
		return Boolean.TRUE.equals( inBoolean );
	}

	public static boolean isFalse(Boolean inBoolean) {
		return Boolean.FALSE.equals( inBoolean );
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
	

	// -------------------
	// Stream related
	// -------------------
	public static void close(Closeable inCloseable) {
		close(inCloseable, false);
	}
	
	public static void close(Closeable inCloseable, boolean inFlush) {

		if (inFlush) {
			flush(inCloseable);
		}

		try {

			inCloseable.close();

		} catch (final Exception e) {}
	}
	
	
	// -------------------
	// InputStream related
	// -------------------

	public static void flush(OutputStream inOutputStream) {

		try {

			inOutputStream.flush();

		} catch (final IOException e) {
			// swallow
		}
	}

	public static void flush(Writer inWriter) {

		try {

			inWriter.flush();

		} catch (final IOException e) {
			// swallow
		}
	}

	public static void flush(Object inOut) {

		if (inOut instanceof OutputStream) {

			flush((OutputStream) inOut);
		} else if (inOut instanceof Writer) {

			flush((Writer) inOut);
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
	
	//
	// Json / Gson releated
	//
	
	private static final JsonParser DEFAULT_JSON_PARSER = new JsonParser();
	private static final Gson DEFAULT_JSON_2_STRING = new GsonBuilder().create();
	private static final Gson DEFAULT_JSON_2_STRING_PRETTYPRINT = new GsonBuilder().setPrettyPrinting().create();

	public static JsonObject readJsonObject(File inFile) throws IOException {
		
		final JsonElement je = readJson(  inFile  ).getAsJsonObject();
		
		return je != null && je.isJsonObject() ? je.getAsJsonObject() : null;
	}
	
	public static JsonElement readJson(File inFile) throws IOException {
		
		FileReader fr = null;
		
		try {
			
			return DEFAULT_JSON_PARSER.parse( fr = new FileReader( inFile ) );
			
		} finally {
			close( fr );
		}
	}

	public static String jsonAsString(JsonElement inJson, boolean inPrettyPrint) {
		return inJson == null ? null : (inPrettyPrint ? DEFAULT_JSON_2_STRING_PRETTYPRINT : DEFAULT_JSON_2_STRING).toJson( inJson );
	}

	public static InputStream jsonAsInputStream(JsonObject inJson, boolean inPrettyPrint) {
		return inJson == null ? null : new ByteArrayInputStream( jsonAsString(inJson, inPrettyPrint).getBytes() );
	}

	// -----------------------------------
	// Replacing / Escaping / Beautifying
	// -----------------------------------
	/**
	 * be aware: http://stackoverflow.com/questions/1265282/recommended-method-for-escaping-html-in-java
	 * @param inValue
	 * @return
	 */
	public static String escapeHtml(String inValue) {
		return inValue == null ? null : escapeXml(inValue).replaceAll( "\"" , "&quot;");
	}
	
	public static String escapeXml(String inValue) {
		return inValue == null ? null
				: inValue//
						.replace("&", "&amp;")//
						.replace("<", "&lt;")//
						.replace(">", "&gt;");
	}
	
	public static String replaceCharactersWithDiacritics(String inText) {
		
		if ( inText == null ) { return inText; }
		
		for (Entry<String,String> e : UMLAUT_REPLACEMENTS.entrySet()) {
			inText = inText.replaceAll( e.getKey() , e.getValue()) ;
		}

	    inText = Normalizer.normalize(inText, Normalizer.Form.NFD);
	    inText = DIACRITICS_AND_FRIENDS.matcher( inText ).replaceAll("");
	    
		return inText;
	}
	
	// --------
	// REGEX
	// --------

	/**
	 * <code>null</code>-safe matching
	 * 
	 * @param inValue
	 * @param inRegex
	 * @return
	 */
	public static boolean isMatch(String inValue, String inRegex) {

		try {

			return isMatch(inValue, inRegex == null ? null : Pattern.compile(inRegex));

		} catch (final Exception e) {

			return false;
		}
	}

	public static boolean isNoMatch(String inValue, String inRegex) {

		return !isMatch(inValue, inRegex);
	}
	
	/**
	 * <code>null</code>-safe matching
	 * 
	 * @param inValue
	 * @param inRegex
	 * @return
	 */
	public static boolean isMatch(String inValue, Pattern inPattern) {

		if (inValue == null && inPattern == null) {

			return true;

		} else if (inValue == null && inPattern != null || inValue != null && inPattern == null) {

			return false;
		}

		return inPattern.matcher(inValue).matches();
	}

	public static boolean isNoMatch(String inValue, Pattern inPattern) {

		return !isMatch(inValue, inPattern);
	}
	
	// ----------------------------------------------------------------------------
	// Arrays, Collections, Iterators, Enumerations
	// ----------------------------------------------------------------------------
	
	@SuppressWarnings("unchecked")
	public static <T> List<T> reverseList(List<T> inList, boolean inReturnNewInstance) {

		if (inList == null) {

			return null;

		} else if (inReturnNewInstance) {

			final List<T> theNewList = ReflectionUtils.newInstance(inList.getClass(), false);
			theNewList.addAll(inList);
			inList = theNewList;
		}

		Collections.reverse(inList);

		return inList;
	}
	
	public static <T> List<T> toList(Enumeration<T> inEnum) {

		if (inEnum == null) {
			return null;
		}

		final List<T> r = new ArrayList<T>();

		while (inEnum.hasMoreElements()) {

			r.add(inEnum.nextElement());
		}

		return r;
	}
	
	public static <T> Iterator<T> toIterator(final Enumeration<T> inEnum) {
		
		return new Iterator<T>() {

			@Override
			public boolean hasNext() {
				return inEnum != null && inEnum.hasMoreElements();
			}

			@Override
			public T next() {
				
				if ( !hasNext() ) {
					throw new IllegalStateException("no more elements in enumeration: "+ inEnum);
				}
				
				return inEnum.nextElement();
			}
		};
	}
	
	public static <T> Iterable<T> toIterable(final Enumeration<T> inEnum) {
		return new Iterable<T>() {
			@Override
			public Iterator<T> iterator() {
				return toIterator(inEnum);
			}
		};
	}
	
	// ----------------------------
	// Big Decimals
	// ----------------------------
	public static boolean isNegative(BigDecimal inValue) {
		
		return inValue != null && BigDecimal.ZERO.compareTo(inValue) > 0;
	}

	public static boolean isZero(BigDecimal inValue) {

		return inValue != null && BigDecimal.ZERO.compareTo(inValue) == 0;
	}

	public static boolean isLarger(BigDecimal inBase, BigDecimal inCompare) {

		return inBase != null && inCompare != null && inBase.compareTo(inCompare) < 0;
	}

	public static boolean isLargerEqual(BigDecimal inBase, BigDecimal inCompare) {

		return inBase != null && inCompare != null && inBase.compareTo(inCompare) <= 0;
	}

	public static boolean isLess(BigDecimal inBase, BigDecimal inCompare) {

		return inBase != null && inCompare != null && inBase.compareTo(inCompare) > 0;
	}

	public static boolean isLessEqual(BigDecimal inBase, BigDecimal inCompare) {

		return inBase != null && inCompare != null && inBase.compareTo(inCompare) >= 0;
	}
	
	// --------------------------------------
	// Text formating
	// --------------------------------------
	public static String formatTime(Long inMillis) {

		if (inMillis == null) {
			return "";
		}

		final boolean isNegative = inMillis < 0;

		inMillis = !isNegative ? inMillis : inMillis * -1;

		final StringBuilder s = new StringBuilder(isNegative ? "-" : "");

		final long theDays = inMillis / MILLIS_1_DAY;

		if (theDays != 0) {

			s.append(theDays).append("d");
			inMillis -= (theDays * MILLIS_1_DAY);
		}

		final long theHours = inMillis / MILLIS_1_HOUR;

		if (theDays != 0 || theHours != 0) {

			s.append(theHours).append("h");
			inMillis -= (theHours * MILLIS_1_HOUR);
		}

		final long theMins = inMillis / MILLIS_1_MINUTE;

		if (theDays != 0 || theHours != 0 || theMins != 0) {

			s.append(theMins).append("m");
			inMillis -= (theMins * MILLIS_1_MINUTE);
		}

		final long theSecs = inMillis / MILLIS_1_SECOND;

		if (theDays != 0 || theHours != 0 || theMins != 0 || theSecs != 0) {

			s.append(theSecs).append("s");
			inMillis -= (theSecs * MILLIS_1_SECOND);
		}

		s.append(inMillis).append("ms");

		return s.toString().trim();
	}

	public static String formatDataSize(Long inSizeBytes) {

		if (inSizeBytes == null) {

			return "";

		} else if (inSizeBytes < 1) {

			return inSizeBytes.toString();
		}

		if (inSizeBytes < 1024) {

			return inSizeBytes + " bytes";
		}

		BigDecimal theSize = new BigDecimal(inSizeBytes).divide(Utils.BD_1024, 1, RoundingMode.HALF_UP);

		if (BD_1024.compareTo(theSize) > 0) {

			return theSize.setScale(2, RoundingMode.HALF_UP).toPlainString() + " KB";
		}

		theSize = theSize.divide(Utils.BD_1024, 10, RoundingMode.HALF_UP);

		if (BD_1024.compareTo(theSize) > 0) {

			return theSize.setScale(2, RoundingMode.HALF_UP).toPlainString() + " MB";
		}

		theSize = theSize.divide(Utils.BD_1024, 10, RoundingMode.HALF_UP);

		if (BD_1024.compareTo(theSize) > 0) {

			return theSize.setScale(2, RoundingMode.HALF_UP).toPlainString() + " GB";
		}

		theSize = theSize.divide(Utils.BD_1024, 10, RoundingMode.HALF_UP);

		if (BD_1024.compareTo(theSize) > 0) {

			return theSize.setScale(2, RoundingMode.HALF_UP).toPlainString() + " TB";
		}

		theSize = theSize.divide(Utils.BD_1024, 10, RoundingMode.HALF_UP);

		return theSize.setScale(2, RoundingMode.HALF_UP).toPlainString() + " PB";
	}
}