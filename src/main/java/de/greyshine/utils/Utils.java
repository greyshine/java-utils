package de.greyshine.utils;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.Flushable;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

import de.greyshine.utils.deprecated.IFileTraverser.Traverser;

public abstract class Utils {

	private static final Log LOG = LogFactory.getLog(Utils.class);
	
	public static final File BASEDIR = getCanonicalFile(new File("."), false); 
	public static final String BASEPATH = BASEDIR.getAbsolutePath(); 
	public static final String BASEURL = execute( new IExecuter<String>() {
		public String run() { 
			try {
				return new File( BASEPATH ).toURI().toURL().toString();
			} catch (MalformedURLException e) {
				throw toRuntimeException(e);
			}
		}
		@Override
		public RuntimeException handleException(Throwable inThrowable) {
			return toRuntimeException(inThrowable);
		}
	}); 
	
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

	public static final BigInteger BI_2 = new BigInteger("2");
	public static final BigInteger BI_3 = new BigInteger("3");
	
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
	/**
	 * https://en.wikipedia.org/wiki/Base58
	 */
	public static final String ALPHABET_BASE58 = "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ";
	
	public static final Pattern DIACRITICS_AND_FRIENDS = Pattern.compile("[\\p{InCombiningDiacriticalMarks}\\p{IsLm}\\p{IsSk}]+");
	
	public static final boolean NEVER_CLOSE_STANDARD_STREAMS = true;
	
	public static Map<String,String> UMLAUT_REPLACEMENTS = new MapBuilder<String,String>()//
			.put("\u00E4", "ae")//
			.put("\u00F6", "oe")//
			.put("\u00FC", "ue")//
			.put("\u00C4", "Ae")//
			.put("\u00D6", "Oe")//
			.put("\u00DC", "Ue")//
			.put("\u00DF", "ss")//
			.get(true);
	
	private static volatile AtomicLong nowAnchor = new AtomicLong(-1L);
	private static volatile long now = System.currentTimeMillis();
	private static final Map<String,DateTimeFormatter> DATETIMEFORMATTERS = new HashMap<>();
	
	private static ToString FUNCTIONS_TOSTRING = new ToString();
	
	/**
	 * http://stackoverflow.com/questions/4448829/regular-expression-for-not-empty
	 */
	public final static String REGEX_NOT_EMPTY = ".*\\S.*";
	
	
	public static final String REGEX_DATE_ISO = "[0-9]{4}\\-[012][0-9]\\-[0123][0-9]";
	public static final String REGEX_TIME_ISO = "[012][0-9]:[0-5][0-9]:[0-5][0-9](\\.[0-9]{3})?";
	/**
	 * e.g. 2011-12-03T10:15:30, 2011-12-03T10:15:30.223 
	 */
	public final static String REGEX_DATETIME_ISO = REGEX_DATE_ISO +"T"+ REGEX_TIME_ISO;
	
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
	
	/**
	 * TODO: make external equal function which can be injected into Utils; just like the toString() methods
	 * @param inO1
	 * @param inO2
	 * @param isBothNullEqual
	 * @return
	 */
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
	
	public static boolean isEqualByteArrays(byte[] in1, byte[] in2, boolean isBothNullEqual) {
		
		if ( in1 == null && in2 == null ) {return isBothNullEqual;  }
		else if ( in1 == in2 ) { return true; }  
		else if ( in1 == null || in2 == null ) { return false; }
		else if ( in1.length != in2.length ) { return false; } 
		for( int i=0, l=in1.length; i<l;i++ ) {
			if ( in1[i] != in2[i] ) { return false; }
		}
		return true;
	}

	public static boolean isEqualClassArrays(Class<?>[] in1, Class<?>[] in2, boolean isBothNullEqual) {
		
		if ( in1 == null && in2 == null ) {return isBothNullEqual;  }
		else if ( in1 == in2 ) { return true; }  
		else if ( in1 == null || in2 == null ) { return false; }
		else if ( in1.length != in2.length ) { return false; } 
		for( int i=0, l=in1.length; i<l;i++ ) {
			if ( in1[i] != in2[i] ) { return false; }
		}
		return true;
	}

	public static boolean notEquals(Object inO1, Object inO2) {

		return !equals(inO1, inO2, true);
	}
	
	public static <T extends Comparable<T>> int compareTo(T t1, T t2) {
		return compareToNullLast(t1, t2);
	}
	
	public static <T extends Comparable<T>> int compareToNullFirst(T t1, T t2) {
		if ( t1 == t2 ) { return 0; }
		else if ( t1 == null ) { return -1; }
		else if ( t2 == null ) { return 1; }
		else { return t1.compareTo( t2 ); }
	}
	
	public static <T extends Comparable<T>> int compareToNullLast(T t1, T t2) {
		if ( t1 == t2 ) { return 0; }
		else if ( t1 == null ) { return 1; }
		else if ( t2 == null ) { return -1; }
		else { return t1.compareTo( t2 ); }
	}
	
	public static boolean isBlank(String inValue) {
		return inValue == null || inValue.trim().isEmpty();
	}
	public static boolean isAnyBlank(String... inValues) {
		if ( inValues != null ) {
			for (String aValue : inValues) {
				if ( isBlank( aValue ) ) {
					return true;
				}
			}
		}
		return false;
	}

	public static boolean isAllBlank(String... inValues) {
		if ( inValues != null ) {
			for (String aValue : inValues) {
				if ( isNotBlank( aValue ) ) {
					return false;
				}
			}
		}
		return true;
	}
	
	public static boolean isNotBlank(String inValue) {
		return !isBlank(inValue);
	}
	
	public static boolean isAnyNotBlank(String... inValues) {
		if ( inValues != null ) {
			for (String aValue : inValues) {
				if ( isNotBlank( aValue ) ) {
					return true;
				}
			}
		}
		return false;
	}
	
	public static boolean isAllNotBlank(String... inValues) {
		if ( inValues != null ) {
			for (String aValue : inValues) {
				if ( isBlank( aValue ) ) {
					return false;
				}
			}
		}
		return true;
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
	
	public static String trimToNull(String inValue) {
		return trimToDefault(inValue, null);
	}
	
	public static String trimToEmpty(String inValue) {
		return trimToDefault(inValue, "");
	}
	public static String trimToEmptyLowerCase(String inValue) {
		return trimToDefault(inValue, "").toLowerCase();
	}
	
	public static String trimToEmptyUpperCase(String inValue) {
		return trimToDefault(inValue, "").toUpperCase();
	}
	
	public List<String> trimAndRemoveBlanks(String[] inArray) {
		
		final List<String> theItems = new ArrayList<>();
		
		if ( inArray != null ) {
			Stream.of( inArray ).map( Utils::trimToNull ) .filter( (s)->s!=null ).forEach( s->{ theItems.add( s ); } );
		}
		
		return theItems;
	}
	
	public static String trimToDefault(String inValue, String inDefault) {
		
		if ( inValue == null ) { return inDefault; }
		
		return (inValue = inValue.trim()).isEmpty() ? inDefault : inValue; 
	}
	
	
	
	public static String unwrap(String inValue, Character inCharacter) {
		
		if (inCharacter == null || inValue.length()<2) { return inValue; }
		
		if ( inCharacter.equals( inValue.charAt( 0 ) ) && inCharacter.equals( inValue.charAt( inValue.length()-1 ) ) ) {
			
			return inValue.substring(1, inValue.length()-1);
		}
		
		return inValue;
	}
	
	/**
	 * Unwraps some text inbetween of the texts before and after.
	 * 
	 * @param inTextToUnwrap
	 * @param inTextBefore
	 * @param inTextAfter
	 * @return
	 */
	public static String unwrap(String inTextToUnwrap, String inTextBefore, String inTextAfter) {
		
		if ( inTextToUnwrap == null || inTextToUnwrap.isEmpty() ) { return null; }
		requireNotNull(inTextBefore, "text before is null");
		requireNotNull(inTextAfter, "text after is null");
		
		int idxBefore = inTextToUnwrap.indexOf( inTextBefore );
		
		if ( idxBefore < 0 ) { return null; }
		
		int idxAfter = inTextToUnwrap.indexOf( inTextAfter, idxBefore+inTextBefore.length() );
		
		return idxAfter < idxBefore ? null : inTextToUnwrap.substring( idxBefore+inTextBefore.length(), idxAfter); 
				
		
	}
	
	public static List<String> grep(String inString, final String... inGreps) {
		
		if ( inString == null || inGreps == null || inGreps.length == 0 ) { return null; }
		
		final List<String> theLines = new ArrayList<>();
		
		for( String aLine : inString.split("\n",-1) ) {
			
			boolean insert = true;
			
			for( String aGrep : inGreps ) {
				
				if ( aGrep == null ) { continue; }
				else if ( !aLine.contains( aGrep ) ) { insert = false; break; }

			}
			
			if ( insert ) {
				theLines.add( aLine );
			}
		}
		
		return theLines;
	}

	public static List<String> egrep(String inString, final String... inEgreps) {
		
		if ( inString == null || inEgreps == null || inEgreps.length == 0 ) { return null; }
		
		final List<String> theLines = new ArrayList<>();
		
		for( String aLine : inString.split("\n",-1) ) {
			
			boolean insert = true;
			
			for( String anEgrep : inEgreps ) {
				
				if ( anEgrep == null ) { continue; }
				else if ( !isMatch( aLine, ".*"+ anEgrep +".*")  ) { insert = false; break; }
				
			}
			
			if ( insert ) {
				theLines.add( aLine );
			}
		}
		
		return theLines;
	}
	
	public static RuntimeException toRuntimeException(Throwable e) {
		return e == null || e instanceof RuntimeException ? (RuntimeException)e : new RuntimeException(e);
	}
	
	public static IOException toIOException(Throwable inThrownException, boolean inCheckCauses) {
	
		if ( inThrownException == null ) { return null; }
		if ( inThrownException instanceof IOException ) { return (IOException)inThrownException; }
		if ( inCheckCauses ) {
			
			final IOException ioe = findException( IOException.class, inThrownException );
			if ( ioe != null ) { return ioe; }
		}
		
		return new IOException( inThrownException );
	}
	
	@SuppressWarnings("unchecked")
	private static <T extends Throwable> T findException(Class<T> inThrowableClassToFind, Throwable inThrowable) {
		
		if ( inThrowable == null || inThrowableClassToFind == null ) { return null; }
		
		final Set<Throwable> checkedThrowables = new HashSet<>();
		
		while( inThrowable != null ) {
			
			if ( inThrowableClassToFind.isAssignableFrom( inThrowable.getClass() ) ) { return (T) inThrowable; }
			
			if ( checkedThrowables.contains( inThrowable ) ) { return null; }
			checkedThrowables.add( inThrowable );
			
			inThrowable = inThrowable.getCause();
		}
		
		return null;
	}

	// ---------------------
	// Require / Validations
	// ---------------------
	
	
	public static String requireNotBlank(String inValue) {
		return requireNotNull(inValue, null);
	}
	
	public static String requireNotBlank(String inValue, String inMessage) {
		
		if ( isBlank(inValue) ) {
			
			throw new IllegalArgumentException( defaultIfBlank( inMessage, "Value must have not have been blank." ) );
		}
		
		return inValue;
	}
	
	public static <T> T requireNotNull(T inValue) {
		return requireNotNull(inValue, null);
	}
	public static <T> T requireNotNull(T inValue, String inMessage) {
		
		if ( inValue == null ) {
			
			inMessage = defaultIfBlank( inMessage, "Value must have not have been null." );
			throw new IllegalArgumentException( inMessage );
		}
		
		return inValue;
	}
	
	public static void requireRegex(String inValue, String inRegex) {
		requireRegex(inValue, inRegex, null);
	}
	
	public static void requireRegex(String inValue, String inRegex, String inMessage) {
		
		if ( inValue == null || isNoMatch(inValue, inRegex) ) { 
			
			inMessage = inMessage != null ? inMessage : "not matching [regex="+ inRegex +", value="+ inValue +"]";
			throw new IllegalArgumentException( inMessage );
		}
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
	
	/**
	 * 
	 * trying to fetch data from the following resources
	 * 
	 * CLASSPATH,
	 * FILE,
	 * URL
	 * 
	 * @param inResource
	 * @return
	 */
	public static InputStream getResource(String inResource) {
		return getResource(inResource, true);
	}

	public static InputStream getResource(String inResource, final boolean inAllowUrlResolution) {

		InputStream theIs = null;
		
		try {
			
			theIs = inResource == null ? null : getResourceUrl(inResource, inAllowUrlResolution).openStream();
		
		} catch (Exception e) {
			// swallow
		}

		return theIs;
		
	}

	public static URL getResourceUrl(String inResource) {
		
		return getResourceUrl(inResource, true);
	}

	public static URL getResourceUrl(String inResource, boolean inAllowUrlResolution) {
		
		if ( inResource == null ) { return null; }
		
		URL theIs = null;
		
		try {
			
			String theResource = inResource;
			theResource = theResource != null && theResource.length() > 0 && theResource.charAt( 0 ) == '/' ? theResource.substring(1) : theResource;
			theIs = Thread.currentThread().getContextClassLoader().getResource( theResource );

		} catch (Exception e) {
			// swallow
		}
		
		try {
			theIs = theIs != null ? theIs : new File( inResource ).toURI().toURL();
		} catch (Exception e) {
			// swallow
		}
		
		try {
			theIs = theIs != null || inAllowUrlResolution == false ? theIs : new URL(inResource);
		} catch (Exception e) {
			// swallow
		}
		
		return theIs;
	}
	
	public static byte[] getResourceAsBytes(String inResource) throws IOException {
		return toBytes( getResource(inResource) );
	}
	
	public static String getResourceAsString(String inResource, Charset inCharset) throws IOException {
		try (InputStream is = getResource(inResource)) {
			return readToString( is , inCharset);
		} 
	}

	public static String getResourceAsStringSafe(String inResource, Charset inCharset, String inDefault) {
		
		try {
		
			try (InputStream is = getResource(inResource)) {
				return readToString( is , inCharset);
			}
			
		} catch (Exception e) {
			// swallow
			return inDefault;
		}
	}
	
	
	public static String castToString(Object inValue) {
		return cast( inValue, (String)null );
	}

	public static <T> T cast(Object inValue) {
		return cast( inValue, null );
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T cast(Object inValue, T inDefault) {
		try {
			return (T)inValue;
		} catch (Exception e) {
			return inDefault;
		}
	}
	
	/**
	 * 
	 * Be aware that a {@link Set} might not be ordered (probably most of the times)!
	 * 
	 * @param inValues
	 * @param inIndex negative index is checked from the end of the collection
	 * @param inDefault
	 * @return
	 */
	public static <T> T getIndexedValueSafe(Set<T> inValues, int inIndex, T inDefault) {
		
		try {
			
			inIndex = inIndex >= 0 ? inIndex : inValues.size() + inIndex;
			
			int i = -1;
			for (T v : inValues) {
				
				i++;
				if ( i == inIndex ) {
					return v;
				}
				
			}
			
		} catch (Exception e) {
			// swallow
		}
		
		return inDefault;
	}
	/**
	 * 
	 * @param inValues
	 * @param inIndex negative index is checked from the end of the collection
	 * @param inDefault
	 * @return
	 */
	public static <T> T getIndexedValueSafe(List<T> inValues, int inIndex, T inDefault) {
		
		try {
		
			inIndex = inIndex >= 0 ? inIndex : inValues.size() + inIndex;
			return inValues.get( inIndex );
			
		} catch (Exception e) {
			// swallow
		}
		
		return inDefault;
	}

	public static <T> T getIndexedValueSafe(T[] inValues, int inIndex, T inDefault) {
		
		try {
			
			inIndex = inIndex >= 0 ? inIndex : inValues.length + inIndex;
			return inValues[inIndex];
			
		} catch (Exception e) {
			// swallow
		}
		
		return inDefault;
	}
	
	// ------------
	// File related
	// ------------
	
	public static File toFile(String inFile) {
		return toFile( inFile, true, true );
	}
	
	public static File toFile(String inFile, boolean inReturnFile, boolean inReturnDir) {
		
		if ( isBlank(inFile) ) { return null; }
		final File f = new File( inFile );
		
		if ( inReturnFile == false && isFile( f ) ) { return null; }
		if ( inReturnDir == false && isDir( f ) ) { return null; }
		
		return f;
	}

	public static boolean isFile(File inFile) {
		return inFile != null && inFile.isFile();
	}
	
	public static boolean isFile(String inFilePath) {
		return isNotBlank( inFilePath ) && isFile( new File( inFilePath ) );
	}
	
	public static boolean isNoFile(File inFile) {
		return !isFile(inFile);
	}
	
	public static boolean isDirectory(File inDir) {
		return isDir( inDir );
	}
	public static boolean isDirectory(String inDir) {
		return isDir( inDir );
	}
	/**
	 * Same as isDirectory()
	 * @param inDir
	 * @return
	 */
	public static boolean isDir(File inDir) {
		return inDir != null && inDir.isDirectory();
	}
	
	public static boolean isDir(String inDirPath) {
		return isNotBlank( inDirPath ) && isDir( new File( inDirPath ) );
	}
	
	public static boolean isNoDir(File inDir) {
		return !isDir(inDir);
	}
	
	public static String getFileType(File inFile) {

		if (inFile == null) {
			return null;
		}

		final int theIdx = inFile.getName().lastIndexOf('.');
		return theIdx < 0 ? null : inFile.getName().substring(theIdx + 1);
	}
	

	public static File getCanonicalFile(String inPath) {
		return getCanonicalFile( inPath, false);
	}
	
	public static String getCanonicalPath(String inPath) {
		return isBlank( inPath ) ? inPath : getCanonicalFile( inPath, false).getAbsolutePath();
	}

	public static File getCanonicalFile(String inPath, boolean inThrowRuntimeException) {
		return isBlank( inPath ) ? null : getCanonicalFile( new File( inPath ), inThrowRuntimeException );
	}
	
	public static boolean isCanonicalPathLocation(File inBaseDir, File inFileToCheck) {
	
		if ( inBaseDir == null || inBaseDir.isFile() || inFileToCheck == null ) {
			return false;
		}
		
		try {
			
			final String theBaseDir = inBaseDir.getCanonicalPath();
			final String theCheckFile = inFileToCheck.getCanonicalPath();
			
			return theCheckFile.startsWith( theBaseDir );
			
		} catch (IOException e) {
			// swallow
			return false;
		}
	}
	
	public static LocalDateTime getFileCreated(String inId) {

		final File f = inId == null ? null : new File(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {

			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.creationTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());

		} catch (IOException e) {

			throw new RuntimeException(e);
		}
	}

	public static LocalDateTime getFileUpdated(String inId) {
		
		final File f = inId == null ? null : new File(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {

			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.lastModifiedTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());

		} catch (IOException e) {

			throw new RuntimeException(e);
		}
	}

	public static LocalDateTime getFileLastAccessed(String inId) {
		
		final File f = inId == null ? null : new File(inId);
		if ( f == null || f.exists() ) { return null; }
		
		try {
			
			final BasicFileAttributes attr = Files.readAttributes(f.toPath(), BasicFileAttributes.class);
			final FileTime time = attr.lastAccessTime();
			return LocalDateTime.ofInstant(time.toInstant(), ZoneId.systemDefault());
			
		} catch (IOException e) {
			
			throw new RuntimeException(e);
		}
	}
	
	/**
	 * 
	 * @param inFile
	 * @return <code>true</code> if the file does not exist, otherwise false
	 */
	public static boolean delete(File inFile) {
		
		if ( inFile == null ) { return true; }
		else if ( isFile( inFile ) ) {
			inFile.delete();
			return !inFile.exists();
		
		}
		final List<File> theDirs = new ArrayList<>();
		theDirs.add( inFile );
		
		while( !theDirs.isEmpty() ) {
			
			File theFile = theDirs.remove( theDirs.size()-1 );
			
			final List<File> theFiles = Utils.list( theFile );
			
			if ( theFiles.isEmpty() ) {
				
				theFile.delete();
			
			} else {
				
				theDirs.add( inFile );
				theFiles.stream().forEach( (aFile)->{
					
					if ( Utils.isFile( aFile ) ) { 
						
						aFile.delete();
						
					} else { theDirs.add( aFile ); }
				} );
				
			}
			
			
			
			
		}
		
		return !inFile.exists();
	}
	
	/**
	 * Same as <code>getCanonicalFile(File inFile, boolean inThrowRuntimeException)</code> but parameter <code>inThrowRuntimeException</code> is set to <code>false</code> 
	 * @param inFile
	 * @return
	 */
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

	
	public static void traversFiles( File inFile, boolean isConsumeDirs, boolean isConsumeFiles, Consumer<File> inConsumer) {
		
		traversFiles(inFile, (aFile)->{
			
			if ( !isConsumeDirs && isDir( aFile ) ) { return true; }
			if ( !isConsumeFiles && isFile( aFile ) ) { return true; }
			
			try {
				
				inConsumer.accept(aFile);
				
			} catch (Exception e) {
				// swallow
			}
			
			return true;
			
		}, true);
	}
	
	public static void traversFiles( File inFile, Consumer<File> inConsumer) {
		
		traversFiles(inFile, (aFile)->{
			
			try {
				
				inConsumer.accept(aFile);
				
			} catch (Exception e) {
				// swallow
			}
			
			return true;
			
		}, true);
	}
	
	/**
	 * The result needs to have a {@link Boolean} result: when <code>false</code> it will quit traversing, otherwise not which inlcudes <code>null</code> as a result value
	 * @param inFile
	 * @param inConsumer
	 * @param inContinueOnException
	 */
	public static void traversFiles( File inFile, Function<File, Boolean> inConsumer, boolean inContinueOnException ) {
		
		if ( inFile == null ) { return; }
		if ( inConsumer == null ) { return; }
		
		final List<File> q = new ArrayList<>();
		q.add( inFile );
		
		while( !q.isEmpty() ) {
			
			final File f = q.remove(0);
			Boolean doContinue = true;
			
			try {
			
				doContinue = inConsumer.apply( f );	
			
			} catch (Exception e) {
				if ( !inContinueOnException ) {
					q.clear();
					throw toRuntimeException(e);
				}
			}
			
			if ( isFalse( doContinue ) ) {
				q.clear();
				break;
			}
			
			if ( f.isDirectory() ) {
				
				Stream.of( defaultIfNull( f.listFiles(), EMPTY_FILES) ).forEach( q::add );
				continue;
			} 
		}
	}
	
	/**
	 * @param inTargetFile
	 * @return the target file if the parent dir exists, otherwise <code>null</code>
	 */
	public static File mkParentDirs(String inTargetFile) {
		if ( isBlank( inTargetFile ) ) { return null; }
		final File theFile = new File( inTargetFile );
		mkParentDirs( theFile );
		return isDir( theFile.getParentFile() ) ? theFile : null;
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
	public static void requireExistingDir(File inDir, String inExceptionMessage) {
		if ( isNoDir(inDir) ) {
			throw new IllegalStateException(inExceptionMessage);
		}
	}
	
	public static void requireExistingFile(File inFile, String inExceptionMessage) {
		if ( isNoFile(inFile ) ) {
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
	
	public static String readFileToString(String inFile, Charset inCharset) throws IOException {
	
		return readToString(inFile, inCharset);
	}
	
	public static String readToString(String inFile, Charset inCharset) throws IOException {
		
		if ( !isFile(inFile) ) { return null; }
		
		try(FileInputStream fis = new FileInputStream( inFile )) {
			return readToString( fis, inCharset);
		}
	}
	
	public static int writeFile(File inFile, String inValue) throws IOException {
	
		return writeFile( inFile, inValue, CHARSET_UTF8 );
	}
	
	public static int writeFile(File inFile, String inValue, Charset inCharset) throws IOException {
		
		mkParentDirs( inFile );

		inValue = inValue == null ? "" : inValue;
		inCharset = inCharset == null ? CHARSET_UTF8 : inCharset;
		
		final byte[] bytes = inValue.getBytes(inCharset); 
		
		try (FileOutputStream fos = new FileOutputStream(inFile)) {
			
			fos.write( bytes );
			fos.flush();
		} 
		
		return bytes.length;
	}
	
	public static String readFileToString(File inFile, Charset inCharset) throws IOException {
		return readToString(inFile, inCharset);
	}
	
	public static Integer readFileToInteger( File inFile, Integer inDefault ) throws IOException {
		
		if ( !Utils.isFile(inFile) ) { return inDefault; }
		
		final String theText = readFileToString(inFile, null);
		
		return Utils.parseInteger(theText, inDefault);
	}
	
	public static String readToString(File inFile, Charset inCharset) throws IOException {
		
		if ( !isFile(inFile) ) { return null; }
		
		try(FileInputStream fis = new FileInputStream( inFile )) {
			return readToString( fis, inCharset);
		}
	}
	
	public static Properties loadProperties(File inFile) throws IOException {
		
		final Properties p = new Properties();
		
		try( InputStream theIs = new FileInputStream( inFile ) ) {
			
			p.load( theIs );
		}
		
		return p;
	}
	
	public static Properties loadProperties(String inResource) throws IOException {
		
		final InputStream theIs = getResource(inResource);
		
		if ( theIs == null ) { return null; }
		
		final Properties p = new Properties();
		
		try {
			p.load( theIs );
		} finally {
			close( theIs );
		}
		
		return p;
	}
	
	/**
	 * Same as read to String
	 * @param inputStream
	 * @param inCharset
	 * @return
	 * @throws IOException
	 */
	public static String inputStreamToString(InputStream inputStream, Charset inCharset, boolean inCloseInputStream) throws IOException {
		try {
			return readToString(inputStream, inCharset);	
		} finally {
			if ( inCloseInputStream ) {
				close( inputStream );
			}
		}
	}

	/**
	 * Same as readToBytes(...)
	 * @param inIs
	 * @param inCloseAfterRead
	 * @return
	 * @throws IOException
	 */
	public static byte[] inputStreamToBytes(InputStream inIs, boolean inCloseAfterRead) throws IOException {
		return readToBytes( inIs, inCloseAfterRead );
	}
	
	/**
	 * Same as toBytes(...)
	 * @param inIs
	 * @param inCloseAfterRead
	 * @return
	 * @throws IOException
	 */
	public static byte[] readToBytes(InputStream inIs, boolean inCloseAfterRead) throws IOException {
		return toBytes(inIs, inCloseAfterRead);
	}
	
	public static void write( File inFile, byte[] inBytes) throws IOException {
		
		mkParentDirs(inFile);
		
		try(FileOutputStream fos = new FileOutputStream( inFile ) ) {
			fos.write( inBytes );
			fos.close();
		}
	}
	
	public static void write( String inFile, byte[] inBytes) throws IOException {
		
		final File theFile = new File( inFile );
		
		mkParentDirs(theFile);
		
		try(FileOutputStream fos = new FileOutputStream( theFile ) ) {
			fos.write( inBytes );
			fos.close();
		}
	}
	
	public void write( File inFile, InputStream inIs) throws IOException {
		copy(inIs, inFile);
	}

	public static String readToString(InputStream inputStream, Charset inCharset) throws IOException {
		
		inCharset = defaultIfNull(inCharset, Charset.defaultCharset());
		
		final Reader r = new InputStreamReader( inputStream, inCharset);
		
		final StringBuilder theSb = new StringBuilder();
		
		while( r.ready() ) {
			theSb.append( (char)r.read() );
		}
		
		return theSb.toString();
	}
	
	/**
	 * List files in the give directory. no further traversing into sub directories.
	 * @param inFile
	 * @return
	 */
	public static List<File> list(File inFile) {
		return list( inFile, (f)->{return true;} );
	}
	
	public static List<File> list(File inFile, Predicate<File> inFilter) {

		if ( !Utils.isDir(inFile) ) {
			return new ArrayList<File>(0);
		}
		
		inFilter = defaultIfNull(inFilter, (x)->{return true;} );

		final List<File> theFiles = new ArrayList<>();

		for (final File aFile : defaultIfNull(inFile.listFiles(), EMPTY_FILES)) {

			if ( !inFilter.test( aFile ) ) { continue; }
			
			theFiles.add(aFile);
		}

		Collections.sort(theFiles, FILE_COMPARATOR);

		return theFiles;
	}
	
	/**
	 * Lists only files; no traversal.
	 * @param inFile
	 * @return
	 */
	public static List<File> listFiles(File inFile) {
		return list( inFile, (f)->{ return Utils.isFile(inFile); } ); 
	}
	
	/**
	 * Lists only directories; no traversal.
	 * @param inFile
	 * @return
	 */
	public static List<File> listDirs(File inFile) {
		return list( inFile, (f)->{ return Utils.isDir(inFile); } ); 
	}

	/**
	 * List all {@link File}s (not directories) in the given directory.
	 * 
	 * @param inFile
	 * @return
	 */
	public static List<File> listFiles(File inDir, boolean inTraversSubDirectories) {

		return listFiles(inDir, inTraversSubDirectories, (file)->{return true;});
	}
	
	public static List<File> listFiles(File inDir, Object inTraversSubDirectories, Function<File,Boolean> inFilter) {
		
		if (inDir == null || !inDir.isDirectory()) {

			return new ArrayList<File>(0);

		}
		
		inFilter = defaultIfNull( inFilter , (file)->{return true;} );

		final List<File> theFiles = new ArrayList<File>(0);

		for (final File aFile : defaultIfNull(inDir.listFiles(), EMPTY_FILES)) {

			if (aFile.isFile() && Boolean.TRUE.equals( inFilter.apply( aFile ) ) ) {

				theFiles.add(aFile);
			}
		}

		Collections.sort(theFiles, FILE_COMPARATOR);

		return theFiles;
		
	}

	/**
	 * List all directories within the given directory.
	 * 
	 * @param inDir
	 *            root dir
	 * @return
	 */
	public static List<File> listDirectorys(File inDir, boolean inTraversSubDirectories) {

		if (inDir == null || !inDir.isDirectory()) {

			return new ArrayList<File>(0);

		}

		final List<File> theFiles = new ArrayList<File>(0);

		for (final File aFile : defaultIfNull(inDir.listFiles(), EMPTY_FILES)) {

			if (aFile.isDirectory()) {

				theFiles.add(aFile);
			}
		}

		Collections.sort(theFiles, FILE_COMPARATOR);

		return theFiles;
	}
	
	// 
	
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
		
		if ( inInputStream == null || inOutputStream == null ) { return -1; }
		
		
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
	
	public static long copy(InputStream inInputStream, File inTargetFile, boolean inCloseInputStream) throws IOException {
		return copy( inInputStream, new FileOutputStream( inTargetFile ), inCloseInputStream, true );
	}
	
	public static long copy(InputStream inInputStream, File inTargetFile) throws IOException {
		return copy( inInputStream, inTargetFile, true ); 
	}
	
	public static long copy(File inSourceFile, File inTargetFile) throws IOException {
		
		if ( isNoFile( inSourceFile ) ) {
			return -1;
		}
		
		return copy( new FileInputStream( inSourceFile ), new FileOutputStream( inTargetFile ), true, true );
	}
	
	public static long copyFile(File inSrcFile, File inTargetFile) throws IOException {
		return copyFile( inSrcFile, inTargetFile, true );
	}
	
	public static long copyFile(File inSrcFile, File inTargetFile, boolean inOverwrite) throws IOException {

		if (inSrcFile == null || !inSrcFile.exists()) {

			return 0;

		} else if (!inSrcFile.isFile()) {

			long theSize = 0;

			for (final File aFile : copyFiles(inSrcFile, inTargetFile, inOverwrite)) {

				theSize += aFile.length();
			}

			return theSize;

		} else if (inTargetFile == null) {

			throw new IOException("Target file is not specified: " + inSrcFile);
		}

		inTargetFile = inTargetFile.isDirectory() ? new File(inTargetFile, inSrcFile.getName()) : inTargetFile;

		inTargetFile.getParentFile().mkdirs();

		if (!inTargetFile.getParentFile().isDirectory()) {

			throw new IOException("Target directory is not accessible: " + inTargetFile.getParent());

		} else if (inTargetFile.exists() && !inOverwrite) {

			throw new IOException("Target file exists: " + inTargetFile);
		}

		FileInputStream theFis = null;
		FileOutputStream theFos = null;

		long theBytes = 0;

		try {

			theBytes = copy(theFis = new FileInputStream(inSrcFile), theFos = new FileOutputStream(inTargetFile));

			if (LOG.isDebugEnabled()) {

				LOG.debug("copy: " + getCanonicalFile(inSrcFile, true) + " > "
						+ getCanonicalFile(inTargetFile, true) + "; " + formatDataSize(theBytes));
			}

			return theBytes;

		} finally {

			close(theFis, theFos);
		}
	}
	
	/**
	 * @param inSrcFile
	 * @param inTargetFile
	 * @param inOverwrite
	 * @return the target files
	 * @throws IOException
	 */
	public static List<File> copyFiles(final File inSrcFile, final File inTargetFile, final boolean inOverwrite)
			throws IOException {

		final List<File> theFiles = new ArrayList<File>();

		if (inSrcFile == null || !inSrcFile.exists()) {

			return theFiles;
		}

		final int theNamecutLength = inSrcFile.getCanonicalPath().length();

		final Traverser t = new Traverser(true) {

			@Override
			public boolean handleDirStart(File inDir) throws Exception {
				
				final File theTargetDir = new File(inTargetFile,
						inDir.getCanonicalPath().substring(theNamecutLength));
				
				theTargetDir.mkdirs();
				
				return theTargetDir.exists();
			}

			@Override
			public boolean handleFile(File srcFile) throws Exception {

				final File theTargetFile = new File(inTargetFile,
						srcFile.getCanonicalPath().substring(theNamecutLength));

				copyFile(srcFile, theTargetFile, inOverwrite);

				theFiles.add(theTargetFile);

				return true;
			}
		};
		
		t.travers( inSrcFile , false);

		if ( t.getException() != null ) {
			throw t.getException() instanceof IOException ? (IOException)t.getException() : new IOException( t.getException() );
		}
		
		return theFiles;
	}
	
	// --------------------------
	// Console
	// --------------------------
	public static Exception runConsoleQuietly(File inDirectory, String... inArgs) {

		int theResultCode;

		try {

			theResultCode = runConsole(inDirectory, DEV0, inArgs);

			if (theResultCode != 0) {

				throw new UnsupportedOperationException("running command returned " + theResultCode);
			}

		} catch (IOException | InterruptedException e) {

			return e;
		}

		return null;
	}

	public static String runConsole(File inDirectory, String... inArgs) throws IOException, InterruptedException {

		final ByteArrayOutputStream theOutBaos = new ByteArrayOutputStream();
		final ByteArrayOutputStream theErrBaos = new ByteArrayOutputStream();

		final int theResultCode = runConsole(inDirectory, theOutBaos, theErrBaos, inArgs);

		if (theResultCode != 0) {

			throw new IOException("Operation failed [code=" + theResultCode + ", args=" + Arrays.toString(inArgs)
					+ ", dev1=" + theErrBaos.toString() + "]");
		}
		
		final String theResult = theOutBaos.toString();

		return theResult.isEmpty() ? null : theResult;
	}

	public static int runConsole(File inDirectory, OutputStream inOut, String... inArgs)
			throws IOException, InterruptedException {

		return runConsole(inDirectory, inOut, null, null, inArgs);
	}

	public static int runConsole(File inDirectory, OutputStream inOut, OutputStream inErrOut, String... inArgs)
			throws IOException, InterruptedException {

		return runConsole(inDirectory, inOut, inErrOut, null, inArgs);
	}

	public static int runConsole(File inDirectory, OutputStream inOut, OutputStream inErrOut, InputStream inIn,
			String... inArgs) throws IOException, InterruptedException {

		if (inIn != null) {

			throw new IllegalArgumentException("Unsupported read of input");
		}

		if (inArgs == null) {

			inArgs = EMPTY_STRINGS;
		}

		for (int i = 0; i < inArgs.length; i++) {

			inArgs[i] = inArgs[i] == null ? "" : inArgs[i];
		}

		final ProcessBuilder theProcessBuilder = new ProcessBuilder(inArgs);
		theProcessBuilder.directory(inDirectory == null || inDirectory.isFile() ? new File(".") : inDirectory);

		final Process theProcess = theProcessBuilder.start();

		inOut = inOut == null ? System.out : inOut;
		inErrOut = inErrOut == null ? System.err : inErrOut;

		new ConsoleStreamListener(theProcess.getInputStream(), inOut);
		new ConsoleStreamListener(theProcess.getErrorStream(), inErrOut);

		return theProcess.waitFor();
	}

	private static class ConsoleStreamListener {
		/**
		 * @param inInputStream
		 * @param inPrefix
		 *            prefix einer jeden Ausgabezeile
		 */
		public ConsoleStreamListener(final InputStream inInputStream, final OutputStream inOut) {

			new Thread(new Runnable() {

				@Override
				public void run() {

					try {

						int r;

						while ((r = inInputStream.read()) != EOF_STREAM) {

							inOut.write(r);
						}

					} catch (final IOException e) {

						e.printStackTrace();
					}
				}

			}).start();
		}
	}
	
	// --------------------------
	// Parsing
	// --------------------------
	public static boolean isParseableBoolean(String string) {

		return parseBoolean(string) != null;
	}

	public static Boolean parseBoolean(String inString) {

		try {

			if ("true".equalsIgnoreCase(inString)) {

				return true;

			} else if ("false".equalsIgnoreCase(inString)) {

				return false;
			}

		} catch (final Exception e) {
		}

		return null;
	}

	public static Boolean parseBoolean(String inString, Boolean inDefault) {

		return defaultIfNull(parseBoolean(inString), inDefault);
	}

	public static Float parseFloat(String inString) {

		return parseFloat(inString, null);
	}

	public static Float parseFloat(String inString, Float inDefault) {

		try {

			return new BigDecimal(inString.trim()).floatValue();

		} catch (final Exception e) {
		}

		return inDefault;
	}

	public static Long parseLong(String inString) {

		return parseLong(inString, null);
	}

	public static boolean isParseableLong(String string) {

		return parseLong(string, null) != null;
	}

	public static Long parseLong(String inString, Long inDefault) {

		try {

			return Long.parseLong(inString.trim());

		} catch (final Exception e) {

		}

		return inDefault;
	}

	public static boolean isParseableInteger(String string) {

		return parseInteger(string) != null;
	}

	public static Integer parseInteger(String inString) {

		return parseInteger(inString, null);
	}

	public static Integer parseInteger(String inString, Integer inDefault) {

		try {

			return new BigDecimal(inString.trim()).setScale(0, RoundingMode.DOWN).intValue();

		} catch (final Exception e) {
			// swallow
		}

		return inDefault;
	}
	
	public static boolean isParseableDouble(String string) {

		return parseDouble(string) != null;
	}

	public static Double parseDouble(String inValue) {

		return parseDouble(inValue, null);
	}

	public static Double parseDouble(String inValue, Double inDefault) {

		try {

			return new BigDecimal(inValue.trim()).doubleValue();

		} catch (final Exception e) {
			return inDefault;
		}
	}

	public static boolean isParseableBigDecimal(String string) {

		return parseBigDecimal(string) != null;
	}

	public static BigDecimal parseBigDecimal(String inString) {

		return parseBigDecimal(inString, null);
	}

	public static BigDecimal parseBigDecimal(String inString, BigDecimal inDefault) {

		try {

			return new BigDecimal(inString);

		} catch (final Exception e) {

			return inDefault;
		}
	}

	public static boolean isParseableCharacter(String string) {

		return parseCharacter(string) != null;
	}

	public static Character parseCharacter(String inString) {

		return parseCharacter(inString, null);
	}

	public static Character parseCharacter(String inString, Character inDefault) {

		Character c = inDefault;

		if (inString != null && inString.length() == 1) {

			c = inString.charAt(0);
		}

		return c;
	}
	
	// -------------------
	// IOStream related
	// -------------------
	public static final int EOF_STREAM = -1;
	
	public static final OutputStream DEV0 = new OutputStream() {@Override public void write(int arg0) throws IOException {}; public String toString() { return "DEV0-OutputStream"; } };
	
	public static void close(Closeable... inCloseable) {
		if ( inCloseable == null ) { return; }
		Arrays.stream( inCloseable ).forEach( (c)->{ close( c, false ); } );
	}
	
	public static void close(Closeable inCloseable, boolean inFlush) {

		if (inFlush && inCloseable instanceof Flushable) {
			flush((Flushable)inCloseable);
		}

		try {

			if ( NEVER_CLOSE_STANDARD_STREAMS && (inCloseable == System.out || inCloseable == System.err || inCloseable == System.in) ) {
				return;
			}
			
			inCloseable.close();

		} catch (final Exception e) {}
	}
	
	public static void flush(Flushable inStream) {

		try {

			inStream.flush();

		} catch (final IOException e) {
			// swallow
		}
	}
	
	public static byte[] toBytes(InputStream inInputStream) throws IOException {
		return toBytes(inInputStream, true);
	}
	
	public static byte[] toByteArray(InputStream inInputStream) throws IOException {
		return toBytes(inInputStream);
	}
	
	public static byte[] toBytes(InputStream inInputStream, boolean inCloseInputStream) throws IOException {
		if ( inInputStream == null ) { return null; }
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		copy( inInputStream, baos, true, true );
		return baos.toByteArray();
	}
	

	/**
	 * Stream which redirects read data from Output- to Inputstream.
	 * 
	 * TODO: write tests 
	 *
	 */
	public static class OutputInputStreams {
		private final List<Integer> data =  new ArrayList<>();
		private boolean closedOutput = false;
		private boolean closedInput = false;
		
		public OutputStream outputStream = new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				
				synchronized (data) {
					
					if ( closedOutput ) {
						throw new IOException("strem already closed");
					}
					
					if ( !closedInput ) {
						data.add( b );
						data.notifyAll();
					}
				}
			}
			
			@Override
			public void close() throws IOException {
				synchronized (data) {
					closedOutput = true;
				}
			}
		};
		
		public InputStream inputStream = new InputStream() {
			
			@Override
			public int read() throws IOException {
				
				synchronized ( data ) {
				
					// more data could come but it is not there yet
					while( !closedOutput && data.isEmpty() ) {
						try {
							wait( 1500 );
						} catch (InterruptedException e) {
							// swallow
						}
					}
					
					// input stream has been closed, so reading data is indicated to be finished.  EOF is returned
					if ( closedInput ) { return -1; }

					// data is not available and no more data will come
					if ( data.isEmpty() && closedOutput ) {
						return -1;
					}
					
					return data.remove(0);
				}
			}
			
			@Override
			public long skip(long n) throws IOException {
				
				long skips = 0;
				
				while( n-->0 ) {
					
					int r = read();
					
					if ( r == -1 ) {
						break;
					}
					
					skips++;
				}
				
				return skips;
			}

			@Override
			public int available() throws IOException {
				synchronized (data) {
					return data.size();	
				}
			}

			@Override
			public void close() throws IOException {
				closedInput = true;
				synchronized ( data ) {
					data.clear();
				}
			}

			@Override
			public synchronized void mark(int readlimit) {
				throw new UnsupportedOperationException("mark not supported");
			}

			@Override
			public boolean markSupported() {
				return false;
			}
			
			
			
		};

		public int getBufferSize() {
			return data.size();
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
	
	public static JsonObject readJsonObject(InputStream inIs, boolean inCloseStream) throws IOException {
		
		final JsonElement je = readJson(  inIs, inCloseStream ).getAsJsonObject();
		
		return je != null && je.isJsonObject() ? je.getAsJsonObject() : null;
	}
	
	public static JsonElement readJson(InputStream inIs, boolean inCloseStream) throws IOException {
		
		try {
			
			return DEFAULT_JSON_PARSER.parse( new InputStreamReader( inIs, CHARSET_UTF8 ) );
			
		} finally {
			
			close( inIs );
		}
	}
	
	public static JsonElement readJson(String inJsonString) throws IOException {
		
		inJsonString = defaultIfBlank(inJsonString, "null");
		
		return readJson( new ByteArrayInputStream( inJsonString.getBytes( CHARSET_UTF8 ) ), true );
	}
	
	public static JsonElement readJson(File inFile) throws IOException {
		
		FileReader fr = null;
		
		try {
			
			return DEFAULT_JSON_PARSER.parse( fr = new FileReader( inFile ) );
			
		} finally {
			close( fr );
		}
	}
	
	public static void writeJson(JsonObject json, File inJsonFile) throws IOException {
		writeJson( json, new FileOutputStream( inJsonFile ), true );
	}
	public static void writeJson(JsonElement json, OutputStream inOs, boolean inCloseStream) throws IOException {
		
		json = json == null ? JsonNull.INSTANCE : json;
		
		 final Gson gson = new GsonBuilder().setPrettyPrinting().create();
		
		 for(byte b : gson.toJson( json ).getBytes( CHARSET_UTF8 )) {
			 
			 inOs.write( b );
		 }
		inOs.flush();
		if ( inCloseStream ) {
			inOs.close();
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

	public static String getSha1(File inFile) {

		if (inFile == null) {

			return null;

		} else if (!inFile.isFile()) {

			throw new IllegalArgumentException("file is no file: " + inFile);
		}


		try (InputStream theIs = new FileInputStream(inFile)) {

			return getSha1(theIs);
			
		} catch (IOException e) {
			
			throw toRuntimeException( e );
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
	
	public static String getSha256(File inFile) {
		
		if (inFile == null) {
			
			return null;
			
		} else if (!inFile.isFile()) {
			
			throw new IllegalArgumentException("file is no file: " + inFile);
		}
		
		
		try ( final InputStream theIs = new FileInputStream(inFile) ) {
			
			return getSha256(theIs);
			
		} catch (Exception e) {
			throw toRuntimeException(e);
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
	
	public static int getHash(String inValue) {

		if (inValue == null) {
			return -1;
		}

		return Integer.parseInt(getHash(inValue, 6, ALPHABET_0to9));
	}

	public static String getHash(String inValue, int inLen, String inAlphabet) {

		try {

			return inValue == null ? null : getHash(inValue.getBytes("UTF-8"), inLen, inAlphabet.toCharArray());

		} catch (final UnsupportedEncodingException e) {

			throw new RuntimeException(e);
		}
	}

	public static String getHash(byte[] inValues, int inLen, char[] inAlphabet) {

		if (inLen < 1 || inAlphabet.length == 0 || inValues == null || inValues.length < 1) {

			return "";
		}

		// System.out.println("inValues.length=" + inValues.length);
		// System.out.println("inAlphabet.length=" + inAlphabet.length);

		final char[] theChars = new char[inLen];
		int idxValues = 0;
		final int theAmtIters = Math.max(inValues.length, theChars.length);

		for (int i = 0; i < theAmtIters; i++, idxValues++) {

			// System.out.println("\ni=" + i);

			idxValues = idxValues % inValues.length == 0 ? 0 : idxValues;

			final int idxHash = i % theChars.length;
			idxValues = idxValues == inValues.length ? 0 : idxValues;

			// System.out.println("idxValues=" + idxValues);
			// System.out.println("idxHash=" + idxHash);

			int idxAlphabet = inValues[idxValues] * (i + 1) * 13;

			idxAlphabet *= 1 + i + theChars[idxHash == 0 ? theChars.length - 1 : idxHash - 1];

			idxAlphabet = Math.abs(idxAlphabet) % inAlphabet.length;
			// System.out.println("idxAlphabet.final=" + idxAlphabet);
			final char theChar = inAlphabet[idxAlphabet];

			theChars[idxHash] = theChar;
		}
		
		return new String(theChars);
	}
	
	public static String toBase64(byte[] inBytes) {
		return inBytes == null ? null : new String(Base64.getEncoder().encode( inBytes ));	
	}
	
	public static String toBase64(InputStream inIs, boolean inCloseStream) throws IOException {
		
		final StringWriter w = new StringWriter();
		
		final OutputStream os = Base64.getEncoder().wrap( new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				w.append( (char)b );
			}
		} );
		
		Utils.copy(inIs, os);
		
		if ( inCloseStream ) {
			close(inIs);
		}
		
		return w.toString();
	}

	public static InputStream toBase64AsInputStream(InputStream inIs, boolean inCloseStream) throws IOException {
		
		final List<Integer> bytes = new ArrayList<>();
		 
		final OutputStream os = Base64.getEncoder().wrap( new OutputStream() {
			@Override
			public void write(int b) throws IOException {
				synchronized ( bytes ) {
					bytes.add( b );
				}
			}
		} );
		
		Utils.copy(inIs, os);
		
		if ( inCloseStream ) {
			close(inIs);
		}
		
		return new InputStream() {
			@Override
			public int read() throws IOException {
				return bytes.size() < 1 ? -1 : bytes.remove(0);
			}
		};
	}
	
	public static void toBase64(InputStream inIs, OutputStream inOs) throws IOException {
		streamBase64( inIs, inOs );
	}
	
	public static void streamBase64(InputStream inIs, OutputStream inOs) throws IOException {
		streamBase64(inIs, inOs, false, false);
	}
	
	public static void streamBase64(InputStream inIs, OutputStream inOs, boolean inCloseInputStream, boolean inCloseOutputStream) throws IOException {
		
		if ( inIs == null || inOs == null ) { return; }
		
		try {
			
			Utils.copy(inIs, inOs = Base64.getEncoder().wrap( inOs ) );
			inOs.flush();
			
		} finally {
			if ( inCloseInputStream ) {
				close(inIs);
			}
			if ( inCloseOutputStream ) {
				close(inOs);
			}
		}
	}
	
	public static String toBase64(InputStream resourceAsStream) throws IOException {
		return toBase64(resourceAsStream, false);
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

	public static boolean isNotZero(BigDecimal inValue) {
		
		return inValue == null || BigDecimal.ZERO.compareTo(inValue) != 0;
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
	
	public static boolean isPrimeNumber(BigDecimal inBd) {
		
		if ( inBd == null || isNotZero( inBd.remainder( BigDecimal.ONE ) ) ) { return false; }
		
		return isPrimeNumber( inBd.toBigInteger() );
	}

	public static boolean isPrimeNumber(BigInteger inBi) {
		
		if ( inBi == null ) { return false; }
		
		if (!inBi.isProbablePrime(5))
	        return false;

		if (!BI_2.equals(inBi) && BigInteger.ZERO.equals(inBi.mod(BI_2)))
	        return false;

	    for (BigInteger i = BI_3; i.multiply(i).compareTo(inBi) < 1; i = i.add(BI_2)) {
	        if (BigInteger.ZERO.equals(inBi.mod(i)))
	            return false;
	    }
	    return true;
		
	}
	
	// --------------------------------------
	// Text formating
	// --------------------------------------
	/**
	 * Formats e.g. into <tt>12d04h0m57s789ms</tt>
	 * @param inMillis
	 * @return
	 */
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
	// Java8-Streams
	// --------------------
	
	public static final Predicate<String> PREDICATE_STRING_NOTBLANK = new Predicate<String>() {
		@Override
		public boolean test(String t) {
			return Utils.isNotBlank( t );
		}
	};
	@SuppressWarnings("rawtypes")
	public static final Predicate<?> PREDICATE_NOTNULL = new Predicate() {
		@Override
		public boolean test(Object t) {
			return t != null;
		}
	};

	public static <T> void forEach(T[] inArray, Consumer<T> inConsumer ) {
		forEach( inArray, inConsumer );
	}
	public static <T> void forEach(T[] inArray, Consumer<T> inConsumer, Predicate<T> inPredicate ) {
		if ( inArray == null || inConsumer == null ) { return; }
		Stream.of( inArray ).filter( inPredicate == null ? inPredicate : i -> {return true;} ).forEach( inConsumer );
	}
	
	
	// ------------
	// Date related
	// ------------
	public void setNow( Long inTimeInMIllis ) {
		
		synchronized ( nowAnchor ) {
		
			if ( inTimeInMIllis == null ) {
				
				nowAnchor.set( -1 );
				
				nowAnchor = null;
				now = System.currentTimeMillis();
			
			} else {
				
				nowAnchor.set( System.currentTimeMillis() );
				now = inTimeInMIllis;
			}
		}
	}
	
	public long getNow() {
		
		if ( nowAnchor.get() < 0 ) {
			
			return System.currentTimeMillis();
		}
		
		synchronized ( nowAnchor ) {
			
			if ( nowAnchor.get() < 0 ) {
				
				return System.currentTimeMillis();
			}

			return now + (System.currentTimeMillis() - nowAnchor.get());
		}
	}
	
	public static LocalDateTime millisToLocalDateTime(Long inMillis) {
		return millisToLocalDateTime(inMillis, ZoneId.systemDefault());
	}
	
	public static LocalDateTime millisToLocalDateTime(Long inMillis, ZoneId inZoneId) {
		
		if ( inMillis == null ) { inMillis = System.currentTimeMillis(); }
		if ( inZoneId == null ) { inZoneId = ZoneId.systemDefault(); }
		
		return LocalDateTime.ofInstant( new Date( inMillis ).toInstant(), inZoneId); 
	}
	
	
	public static String formatDate(String inPattern, LocalDateTime inTime) {
		return formatDate( inPattern, Locale.getDefault(), inTime );
	}
	
	public static String formatDate(String inPattern, Locale inLocale, LocalDateTime inTime) {
		
		if ( inPattern == null || inTime == null ) {
		
			return null;
		}
		
		DateTimeFormatter theDtf = DATETIMEFORMATTERS.get( inPattern );
		
		if ( theDtf == null ) {
		
			DATETIMEFORMATTERS.put( inPattern, theDtf = new DateTimeFormatterBuilder().appendPattern( inPattern ).toFormatter( inLocale ) );
		}
		
		return theDtf.format( inTime );
	}
	
	/**
	 * look at https://stackoverflow.com/a/8854858
	 * 
	 * @param inPattern
	 * @param inDate
	 * @return
	 */
	public static LocalDateTime parseLocalDateTime(String inPattern, String inDate) {
		final DateTimeFormatter f = DateTimeFormatter.ofPattern(inPattern );
		return LocalDateTime.from(f.parse( inDate ));
	}

	public static LocalDate parseLocalDate(String inPattern, String inDate) {
		final DateTimeFormatter f = DateTimeFormatter.ofPattern(inPattern );
		return LocalDate.from(f.parse( inDate ));
	}
	
	public static LocalTime parseLocalTimeDate(String inPattern, String inDate) {
		final DateTimeFormatter f = DateTimeFormatter.ofPattern(inPattern );
		return LocalTime.from(f.parse( inDate ));
	}
	
	// ---------------
	// Thread releated
	// ---------------
	public static void threadNotifyAll( Object inSync ) {
		if ( inSync != null ) {
			synchronized ( inSync ) {
				inSync.notifyAll();
			}	
		}
	}
	
	public static long threadWait( Object inSynchronizedObject, long inMaxCheckInterval, Supplier<Boolean> inWaitCondition ) {
		
		if ( inSynchronizedObject == null ) { return 0; }
		
		final long starttime = System.currentTimeMillis();
		
		inMaxCheckInterval = inMaxCheckInterval <= 0 ? 950 : inMaxCheckInterval;
		
		while( Utils.isTrue( inWaitCondition.get() ) ) {
			
			synchronized ( inSynchronizedObject ) {
				
				try {
					
					inSynchronizedObject.wait( inMaxCheckInterval );
				
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
		
		return System.currentTimeMillis()-starttime;
	}
	
	public static void threadSleep(long inMillis) {
		
		final long theWaitDone = System.currentTimeMillis() + inMillis;
		long theRestTime = theWaitDone - System.currentTimeMillis();
		
		final Thread theCurrentThread = Thread.currentThread(); 
		
		while ( theRestTime > 0 ) {
			
			synchronized ( theCurrentThread ) {
				try {
					Thread.yield();
						theCurrentThread.wait( theRestTime );	
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}

	public static <T,U> U executeQuietly( T inValue, U inDefaultResult, Function<T,U> inFunction ) {
		
 		if ( inValue == null || inFunction == null ) { return inDefaultResult; }
 		
 		try {

 			return inFunction.apply( inValue );
			
		} catch (Exception e) {
			// swallow
			return inDefaultResult;
		}
	}

 	public static <T,U> U executeQuietly( T inValue, Function<T,U> inFunction ) {
 		return executeQuietly(inValue, null, inFunction);
 	}
 	
 	public static <T,U> U executeQuietly(Function<T,U> inFunction ) {
 		return executeQuietly(null, null, inFunction);
 	}
 	


 	public static <T> void executeQuietly(T inValue, Consumer<T> inConsumer ) {
 		
 		try {
 			
 			inConsumer.accept(inValue);
			
		} catch( Exception e ) {
			// swallow
		}
 		 
 	}
	
 	/**
 	 * Handler which always a scoped execution which allows throwing any Exception and offers handling of the exception
 	 * @param <T>
 	 */
	public interface IExecuter<T> {
		T run() throws Exception;
		/**
		 * Any thrown {@link Exception} will be handled as the return value of the method and will be rethrown as a {@link RuntimeException}.<br/>
		 * Defaults to returning <tt>null</tt>
		 * 
		 * @param inThrowable the {@link Exception} previously thrown by the run method
		 * @return if not <tt>null</tt> the result value will be rethrown as a {@link RuntimeException}
		 */
		default RuntimeException handleException(Throwable inThrowable) { return toRuntimeException( inThrowable ); };
	}
	
	public static <T> T execute(IExecuter<T> inExecution) {
		return execute((IExecuter<T>)inExecution, (T)null);
	}
	
	/**
	 * 
	 * @param inExecution
	 * @param inDefaultResult
	 * @return
	 */
	public static <T> T execute(IExecuter<T> inExecution, T inDefaultResult) {
		
		if ( inExecution == null ) {
			return inDefaultResult;
		}
		
		try {
			
			return inExecution.run();
			
		} catch (Throwable t) {

			Throwable t2 = null;
			
			try {
				t2 = inExecution.handleException(t);
			} catch (Exception e) {
				t2 = e;
			}
			
			if ( t2 != null ) {
				throw toRuntimeException(t2);
			}
		}
		
		return inDefaultResult;
	}
	
	// --------------------
	// stuff
	// --------------------
	
	public static long getMemoryUsage(Object... inObjects) {
		return evaluateMemoryUsage(inObjects);
	}
	
	/**
	 * http://www.javamex.com/tutorials/memory/object_memory_usage.shtml
	 * 
	 * @param inObject
	 * @return
	 */
	public static long evaluateMemoryUsage(Object... inObjects) {

		if (inObjects == null || inObjects.length < 1) {
			return 0L;
		}

		final Wrapper<Long> theResult = new Wrapper<Long>(0L);
		final Wrapper<Long> theCurrentObjectSize = new Wrapper<Long>(0L);
		final Set<Object> scannedObjects = new HashSet<Object>();
		final List<Object> theQueue = new ArrayList<Object>();
		
		for(Object anObject : inObjects) {
			if ( anObject != null ) {
				theQueue.add( anObject );
			}
		}
		
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
						// swallow
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

	public static Charset defaultCharset(String inCharset) {
		try {
			return Charset.forName( inCharset );
		} catch (Exception e) {
			return Charset.defaultCharset();
		}
	}

	/**
	 * <code>null</code>-safe array to List.
	 * A List will always be returned.
	 * @param inArgs
	 * @return
	 */
	@SafeVarargs
	public static <T> List<T> toList(T... inArgs) {
		return inArgs == null ? new ArrayList<>(0) : new ArrayList<>( Arrays.asList( inArgs ) );
	}

	public static String markdownToHtml(String inMarkdown) {
		return ShowdownTransformer.toHtml(inMarkdown);
	}

	/**
	 * Simple logger interface for development in order to have an easy way to turn on/off outputs.
	 * Reference the {@link ILogger}.SYSTEM_OUT_ERR for printing to stdout/stderr.
	 */
	public interface ILogger {
		ILogger SYSTEM_OUT_ERR = new ILogger() {
			@Override
			public void log(Object inMessage, Throwable t) {
				System.out.println( inMessage );
				if ( t != null ) { t.printStackTrace( System.out ); }
			}
			@Override
			public void err(Object inMessage, Throwable t) {
				System.err.println( inMessage );
				if ( t != null ) { t.printStackTrace( System.err ); }
			}
		};
		void log(Object inMessage, Throwable t);
		default void log(Object inMessage) { log( inMessage, null ); };
		void err(Object inMessage, Throwable t);
		default void err(Object inMessage) { err( inMessage, null ); };
	}
	
	// ------------------
	// toString() methods
	// ------------------
	public static String toString(Object inObject) {
		return FUNCTIONS_TOSTRING.toString( inObject );
	}
	
	public static void registerToString(Class<?> inClass, Function<Object,String> inToStringFunction) {
		FUNCTIONS_TOSTRING.register( inClass, inToStringFunction);
	}

	public static String concatLines(Collection<String> inLines, String inSeparator) {
		
		if ( inLines == null ) { return ""; }
		
		inSeparator = defaultIfNull(inSeparator, "");
		
		final StringBuilder sb = new StringBuilder();
		
		final Iterator<String> i = inLines.iterator();
	
		while( i.hasNext() ){
			sb.append( i.next() ).append( i.hasNext() ? inSeparator : "" );
		}
		
		return sb.toString();
	}

	public static String encrypt(String inValue, final String inPassword) {
		
		if ( inValue == null || inPassword == null || inPassword.isEmpty() ) { return inValue; }
		
		final StringBuilder theResult = new StringBuilder();
		
		try {
			encrypt(inPassword, new ByteArrayInputStream( inValue.getBytes( CHARSET_UTF8 ) ) , new OutputStream() {
				@Override
				public void write(int b) throws IOException {
					if ( b < 16 ) { theResult.append( '0' ); }
					theResult.append( Integer.toHexString( b ) );
				}
			});

		} catch(IOException e) {
			throw Utils.toRuntimeException( e );
		}
		
		return theResult.toString();
	}
	
	public static String decrypt(String inValue, String inPassword) {
		
		if ( inValue == null || inValue.isEmpty() || inPassword == null || inPassword.isEmpty() ) { return inValue; }
		
		if ( isNoMatch(inValue, "([0-9a-fA-F]{2})*") ) {
			throw new IllegalArgumentException("value is not hex 00-FF");
		}
		
		final ByteArrayOutputStream theResult = new ByteArrayOutputStream();
		
		try {
			
			encrypt(inPassword, new InputStream() {
				
				final int l = inValue.length();
				int idx = 0;
				
				@Override
				public int available() throws IOException {
					return (l - idx) / 2;
				}

				@Override
				public int read() throws IOException {

					if ( idx >= l ) {
						throw new IOException("bad read count");
					}
					
					final int r = Integer.parseInt( inValue.substring(idx, idx+2) , 16);
					
					idx += 2;
					
					return r;
				}
				
			}, theResult );
			
			return theResult.toString( CHARSET_UTF8.name() );

		} catch (IOException e) {
			throw Utils.toRuntimeException( e );
		}
		
	}
	
	public static void encrypt( String inPassword, InputStream inDataStream, OutputStream inResultStream) throws IOException {
		
		if ( inPassword == null || inPassword.isEmpty() ) {
			copy(inDataStream, inResultStream);
			return;
		}
		
		final Iterator<Integer> pwdCharIter = new Iterator<Integer>() {

			private int idx = -1;
			
			@Override
			public boolean hasNext() {
				return true;
			}

			@Override
			public Integer next() {
				
				return (int) inPassword.charAt( ++idx % inPassword.length() );
			}
		};
		
		int b;
		
		while( inDataStream.available() > 0 ) {
			
			b = inDataStream.read();
			
			b ^= pwdCharIter.next();
			
			inResultStream.write( b );
		}

		inResultStream.flush();
	}

	public static String readUrlToString(String inUrl) throws IOException {
		// TODO add timeout
		try ( InputStream is = new URL( inUrl ).openStream() ) {
			return readToString(is, CHARSET_UTF8);	
		} 
	}

	public static JsonElement readUrlToJson(String inUrl) throws IOException {
		// TODO add timeout
		return readJson(new URL( inUrl ).openStream() , true);
	}
	
	public static long getMillis(int inDays, int inHours, int inMinutes, int inSeconds) {

		return getMillis(inDays, inHours, inMinutes, inSeconds, 0);
	}

	public static long getMillis(int inDays, int inHours, int inMinutes, int inSeconds, int inMillis) {

		return (inMillis) + (inSeconds * de.greyshine.utils.Utils.MILLIS_1_SECOND) + (inMinutes * de.greyshine.utils.Utils.MILLIS_1_MINUTE) + (inHours * de.greyshine.utils.Utils.MILLIS_1_HOUR)
				+ (inDays * MILLIS_1_DAY);
	}
	

}