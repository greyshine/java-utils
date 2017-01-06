package de.greyshine.utils;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Writer;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Collection;
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
import java.util.stream.Stream;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;


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
	
	public static boolean isBlank(String inValue) {
		return inValue == null || inValue.trim().isEmpty();
	}
	
	public static boolean isNotBlank(String inValue) {
		return !isBlank(inValue);
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
	
	public static RuntimeException toRuntimeException(Exception e) {
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
	
	public static Throwable findThrowable(Throwable inThrowable, Class<? extends Throwable> inThrowableClass) {

		if (inThrowable == null || inThrowableClass == null) {
			return null;
		}

		Throwable t = inThrowable;
		
		final Set<Throwable> listeds = new HashSet<>(3);

		while (t != null) {

			if (inThrowableClass.isAssignableFrom(t.getClass())) {
				return t;
			}

			t = t.getCause();
			
			if ( listeds.contains( t ) ) {
				t = null;
			}
		}

		return null;
	}
	
	public static List<Throwable> listCauses(Throwable inThrowable) {
		
		final List<Throwable> theCauses = new ArrayList<>();
		
		Throwable t = inThrowable;
		
		while (t != null) {
			
			theCauses.add( t );
			
			t = t.getCause();
			
			if ( theCauses.contains( t ) ) {
				break;
			}
		}
		
		return theCauses;
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
	
	public static String getFileType(File inFile) {

		if (inFile == null) {
			return null;
		}

		final int theIdx = inFile.getName().lastIndexOf('.');
		return theIdx < 0 ? null : inFile.getName().substring(theIdx + 1);
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
	
	public static FileOutputStream createOutputStream(String inFile) {

		return inFile == null ? null : createOutputStream(new File(inFile));
	}

	public static FileOutputStream createOutputStream(File inFile) {

		if (inFile == null || inFile.isDirectory()) {
			return null;
		}

		mkParentDirs(inFile);

		if (!inFile.getParentFile().isDirectory()) {
			return null;
		}

		try {

			return new FileOutputStream(inFile);

		} catch (final Exception e) {
			// swallow
			return null;
		}
	}
	
	public static boolean mkParentDirs(File inTarget) {

		if (inTarget == null) {

			return false;

		} else if (inTarget.exists()) {

			return true;
		}

		return inTarget.getParentFile().mkdirs();
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
	
	public static List<String> readFileToLines(String inFile, String inCharset) throws IOException {
		return inFile == null ? new ArrayList<>(0) : readFileToLines(new File(inFile), inCharset);
	} 
	public static List<String> readFileToLines(File inFile, String inCharset) throws IOException {
		return inFile == null ? new ArrayList<>(0) : readFileToLines(inFile, inCharset== null ? null : Charset.forName( inCharset ));
	} 
	
	public static List<String> readFileToLines(String inFile, Charset inCharset) throws IOException {
		return inFile == null ? new ArrayList<>(0) : readFileToLines(new File(inFile), inCharset);
	} 
	public static List<String> readFileToLines(File inFile, Charset inCharset) throws IOException {
		
		if ( !isFile(inFile) ) { return null; }
		
		inCharset = defaultIfNull(inCharset, Charset.defaultCharset() );
		
		final List<String> theLines = new ArrayList<>();

		try ( BufferedReader r = new BufferedReader( new InputStreamReader( new FileInputStream( inFile ) , inCharset ) ) )  {
		
			while( r.ready() ) {
				theLines.add( r.readLine() );
			}
		} 
		
		return theLines;
	} 
	
	public static long copy(InputStream inInputStream, OutputStream inOutputStream, boolean inCloseStreams)
			throws IOException {

		return copy(inInputStream, inOutputStream, inCloseStreams, inCloseStreams);
	}

	public static long copy(InputStream inInputStream, OutputStream inOutputStream, boolean inCloseInputStream,
			boolean inCloseOutputStream) throws IOException {

		try {

			return copy(inInputStream, inOutputStream);

		} finally {

			if (inCloseInputStream) {

				close(inInputStream);
			}

			if (inCloseOutputStream) {

				close(inOutputStream);
			}
		}
	}

	public static long copy(InputStream inInputStream, OutputStream inOutputStream) throws IOException {
		//
		final byte[] buffer = new byte[4 * 1024];

		long count = 0;
		int n = 0;
		while (EOF_STREAM != (n = inInputStream.read(buffer))) {
			inOutputStream.write(buffer, 0, n);
			count += n;
		}

		inOutputStream.flush();
		return count;
	}
	

	// -------------------
	// Stream related
	// -------------------
	public static final int EOF_STREAM = -1;
	
	public static final OutputStream DEV0 = new OutputStream() {@Override public void write(int arg0) throws IOException {} };
	
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

	public static InputStream jsonAsInputStream(JsonElement inJson, boolean inPrettyPrint) {
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
	
	// -------------------------
	// digesting
	// -------------------------
	public static String getMd5(File inValue) {

		if (inValue == null || !inValue.exists()) {
			return null;
		} else if (inValue.isDirectory()) {
			throw new IllegalArgumentException("file is directory: " + inValue );
		}

		InputStream theIs = null;

		try {

			return getMd5(theIs = new FileInputStream(inValue));

		} catch (final FileNotFoundException e) {

			throw toRuntimeException(e);

		} finally {

			close(theIs);
		}
	}

	public static String getMd5(String inValue) {

		return inValue == null ? null : getMd5(new ByteArrayInputStream(inValue.getBytes()));
	}

	public static String getMd5(InputStream in) {

		try {

			return getDigest("MD5", in);

		} catch (final Exception e) {

			throw toRuntimeException(e);
		}
	}

	public static String getSha1(String inValue) {

		return inValue == null ? null : getSha1(new ByteArrayInputStream(inValue.getBytes()));
	}

	public static String getSha1(File inFile) throws IOException {

		if (inFile == null) {

			return null;

		} else if (!inFile.isFile()) {

			throw new IllegalArgumentException("file is no file: " + inFile);
		}

		final InputStream theIs = new FileInputStream(inFile);

		try {

			return getSha1(theIs);

		} finally {

			close(theIs);
		}
	}

	public static String getSha1(InputStream in) {

		try {

			return getDigest("SHA-1", in);

		} catch (final Exception e) {

			throw toRuntimeException(e);
		}
	}

	public static String getSha256(String inValue) {
		
		return inValue == null ? null : getSha256(new ByteArrayInputStream(inValue.getBytes()));
	}
	
	public static String getSha256(File inFile) throws IOException {
		
		if (inFile == null) {
			
			return null;
			
		} else if (!inFile.isFile()) {
			
			throw new IllegalArgumentException("file is no file: " + inFile);
		}
		
		final InputStream theIs = new FileInputStream(inFile);
		
		try {
			
			return getSha256(theIs);
			
		} finally {
			
			close(theIs);
		}
	}
	
	public static String getSha256(InputStream in) {
		
		try {
			
			return getDigest("SHA-256", in);
			
		} catch (final Exception e) {
			
			throw toRuntimeException(e);
		}
	}

	public static String getDigest(String inAlgorithm, InputStream inIs) throws IOException, NoSuchAlgorithmException {

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
	
	// --------------------
	// Streams
	// --------------------
	
	@SafeVarargs
	public static <T> Stream<T> toStream(T... inValues) {
		
		final List<T> theItems = new ArrayList<>( inValues == null ? 0 : inValues.length );
		
		if ( inValues != null ) {
			for (T anItem : inValues) {
				theItems.add( anItem );
			}
		}

		return theItems.stream();
	}

	@SafeVarargs
	public static <T> Stream<T> toParallelStream(T... inValues) {
		
		final List<T> theItems = new ArrayList<>( inValues == null ? 0 : inValues.length );
		
		if ( inValues != null ) {
			for (T anItem : inValues) {
				theItems.add( anItem );
			}
		}
		
		return theItems.parallelStream();
	}
	
	// --------------------
	// stuff
	// --------------------
	
	public static long evaluateMemoryUsages(Object... inObjects) {
		long theResult = 0;
		if (inObjects != null) {
			for (final Object o : inObjects) {
				theResult += evaluateMemoryUsage(o);
			}
		}
		return theResult;
	}
	
	public static long evaluateMemoryUsages(Collection<?> inObjects) {
		long theResult = 0;
		if (inObjects != null) {
			for (final Object o : inObjects) {
				theResult += evaluateMemoryUsage(o);
			}
		}
		return theResult;
	}

	/**
	 * http://www.javamex.com/tutorials/memory/object_memory_usage.shtml
	 * 
	 * @param inObject
	 * @return
	 */
	public static long evaluateMemoryUsage(Object inObject) {

		if (inObject == null) {
			return 0L;
		}

		final Wrapper<Long> theResult = new Wrapper<Long>(0L);
		final Wrapper<Long> theCurrentObjectSize = new Wrapper<Long>(0L);
		final Set<Object> scannedObjects = new HashSet<Object>();
		final List<Object> theQueue = new ArrayList<Object>();
		theQueue.add(inObject);

		while (!theQueue.isEmpty()) {

			final Object theObject = theQueue.remove(0);

			try {

				if (scannedObjects.contains(theObject)) {

					// object was referenced but also scanned
					continue;
				}

			} catch (final Throwable e) {

				theResult.value += 8;
				continue;
			}

			try {

				scannedObjects.add(theObject);

			} catch (final Throwable e) {
				// swallow
			}

			theCurrentObjectSize.value = 8L;

			final Class<?> theObjectsClass = theObject.getClass();

			if (theObject.getClass().isArray()) {

				theCurrentObjectSize.value += 4;

				final int len = Array.getLength(theObject);
				for (int i = 0; i < len; i++) {

					final Object anArrayObject = Array.get(theObject, i);

					try {

						if (anArrayObject != null && !scannedObjects.contains(anArrayObject)) {

							theQueue.add(anArrayObject);
						}

					} catch (final Throwable e) {
						// TODO: handle exception
					}
				}

			} else {

				try {

					ReflectionUtils.traversFieldHierarchy(theObjectsClass, new FieldHandler() {

						{
							handleStatic = false;
						}

						@Override
						public boolean doHandle(Field inField) {

							Object theValue = null;

							try {

								theValue = ReflectionUtils.getFieldValue(inField, theObject);

							} catch (IllegalArgumentException | IllegalAccessException e) {

								// swallow
							}

							try {

								if (theValue == null || scannedObjects.contains(theValue)) {

									return true;
								}

							} catch (final Throwable e) {
								// swallow
								return true;
							}

							final Class<?> theClass = theValue.getClass();

							if (theClass.isPrimitive()) {

								if (theClass == boolean.class || theClass == byte.class) {

									theCurrentObjectSize.value += 1;

								} else if (theClass == char.class || theClass == short.class) {

									theCurrentObjectSize.value += 2;

								} else if (theClass == int.class || theClass == float.class) {

									theCurrentObjectSize.value += 4;

								} else if (theClass == long.class || theClass == double.class) {

									theCurrentObjectSize.value += 8;

								}

							} else {

								theQueue.add(theValue);
							}

							return true;
						}

					});

				} catch (final Throwable e) {
					// swallow
				}
			} // eof if-else array vs plain object

			// add padding
			final int leftToFull8 = 8 - (int) (theCurrentObjectSize.value % 8);
			if (leftToFull8 != 8) {

				theCurrentObjectSize.value += leftToFull8;
			}

			theResult.value += theCurrentObjectSize.value;

		} // eof while queue

		return theResult.value;
	}
}