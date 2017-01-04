package de.greyshine.utils.deprecated;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.charset.Charset;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.Normalizer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonParser.Feature;
import com.fasterxml.jackson.core.JsonToken;

import de.greyshine.utils.FieldHandler;
import de.greyshine.utils.IHandler;
import de.greyshine.utils.IMapHandler;
import de.greyshine.utils.IdentitySet;
import de.greyshine.utils.MapBuilder;
import de.greyshine.utils.ReflectionUtils;
import de.greyshine.utils.Wrapper;
import de.greyshine.utils.deprecated.IFileTraverser.Traverser;

public abstract class Utils {
	
	private static final Log LOG = LogFactory.getLog(Utils.class);

	public static final int EOF_STREAM = -1;

	public static final OutputStream DEV0 = new OutputStream() {
		@Override
		public void write(int arg0) throws IOException {
		}
	};

	public static final Class<?>[] EMPTY_CLASSES = new Class<?>[0];
	public static final Object[] EMPTY_OBJECTS = new Object[0];
	public static final String[] EMPTY_STRINGS = new String[0];
	public static final byte[] EMPTY_BYTES = new byte[0];
	public static final File[] EMPTY_FILES = new File[0];
	public static final InputStream EMPTY_INPUTSTREAM = new ByteArrayInputStream(new byte[0]);
	public static final InputStream[] EMPTY_INPUTSTREAMS = new InputStream[0];
	public static final Map<String, Object> EMPTY_MAP_STRING_OBJECT = Collections
			.unmodifiableMap(new HashMap<String, Object>(0));
	public static final Map<String, Object[]> EMPTY_MAP_STRING_OBJECTS = Collections
			.unmodifiableMap(new HashMap<String, Object[]>(0));
	public static final Object NULLVALUE = new Object();

	public static final String ALPHABET_0to9 = "0123456789";
	public static final String ALPHABET_HEX = "0123456789abcdef";
	public static final String ALPHABET_SMALL = "0123456789abcdefghijklmnopqrstuvwxyz";
	public static final String ALPHABET_LARGE = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

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

	private static final EResourceType[] DEFAULT_RESOURCE_HIERARCHY = new EResourceType[] { EResourceType.CLASSPATH,
			EResourceType.CONTEXTLOADER_CLASSPATH, EResourceType.FILE, EResourceType.URL };

	public static final String STRING_NULL = "";

	private static Map<String, Wrapper<Long>> COUNTERS = new HashMap<String, Wrapper<Long>>();
	
	public static final Pattern DIACRITICS_AND_FRIENDS = Pattern.compile("[\\p{InCombiningDiacriticalMarks}\\p{IsLm}\\p{IsSk}]+");
	
	private static Map<String,String> UMLAUT_REPLACEMENTS = new MapBuilder<String,String>()//
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
	public final static Pattern REGEX_NOT_EMPTY = Pattern.compile(".*\\S.*");

	public static final String REGEX_VERSION = "[1-9]*[0-9](\\.[0-9]+)*";
	public static final Pattern PATTERN_VERSION = Pattern.compile(REGEX_VERSION);

	private Utils() {
	}

	/**
	 * Will try to find a resource and return the first successful match:<br/>
	 * <ol>
	 * <li>Classpath</li>
	 * <li>iterate new File( inBasePath[] ,inDescriptor); if not provided</li>
	 * <li>new File(".",inDescriptor)</li>
	 * <li>new URL( inDescriptor )</li>
	 * </ol>
	 * 
	 * @param inDescriptor
	 * @return
	 */
	public InputStream getResource(String inDescriptor, File... inBasePaths) {

		try {

			final InputStream theIs = Thread.currentThread().getContextClassLoader().getResourceAsStream(inDescriptor);

			if (theIs != null) {

				return theIs;
			}

		} catch (final Exception e) {
			// swallow
		}

		if (inBasePaths != null) {

			for (final File aBasePath : inBasePaths) {

				try {

					final File aFile = new File(aBasePath, inDescriptor);
					if (aFile.isFile()) {

						return new FileInputStream(aFile);
					}

				} catch (final Exception e) {
					// swallow
				}
			}
		}

		try {

			final File aFile = new File(".", inDescriptor);
			if (aFile.isFile()) {

				return new FileInputStream(aFile);
			}

		} catch (final Exception e) {
			// swallow
		}

		try {

			return new URL(inDescriptor).openStream();

		} catch (final Exception e) {

			// swallow
		}

		return null;
	}

	/**
	 * @param inUrls
	 * @return the {@link InputStream} of first openable URL
	 */
	public InputStream openInputStream(URL[] inUrls) {

		return inUrls == null ? null : openInputStream(Arrays.asList(inUrls));
	}

	/**
	 * @param inUrls
	 * @return the {@link InputStream} of first openable URL
	 */
	public InputStream openInputStream(Collection<URL> inUrls) {

		if (inUrls == null) {
			return null;
		}

		for (final URL url : inUrls) {

			if (url == null) {

				continue;
			}

			try {

				return url.openStream();

			} catch (final Exception e) {
				// swallow
			}
		}
		return null;
	}

	public static Exception runConsoleQuietly(File inDirectory, String... inArgs) {

		int theResultCode;

		try {

			theResultCode = runConsole(inDirectory, Dev0OutputStream.INSTANCE, inArgs);

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

						LOG.error(e);
					}
				}

			}).start();
		}
	}

	/**
	 * Do never catch Throwables!
	 * 
	 * @param e
	 * @return
	 */
	public static RuntimeException toRuntimeException(Exception e) {

		return e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
	}

	public static IOException toIOException(Exception e) {

		return e instanceof IOException ? (IOException) e : new IOException(e);
	}

	public static void throwRuntimeExceptionIfNotNull(Exception inException) {

		if (inException != null) {

			throw Utils.toRuntimeException(inException);
		}

	}

	public static void throwIOExceptionIfNotNull(Exception inException) throws IOException {

		if (inException != null) {

			throw Utils.toIOException(inException);
		}
	}

	public static List<File> list(File inFile) {

		if (inFile == null || !inFile.isDirectory()) {

			return new ArrayList<File>(0);

		}

		final List<File> theFiles = new ArrayList<File>(0);

		for (final File aFile : defaultIfNull(inFile.listFiles(), EMPTY_FILES)) {

			theFiles.add(aFile);
		}

		Collections.sort(theFiles, FILE_COMPARATOR);

		return theFiles;
	}

	/**
	 * List all {@link File}s (not directories) in the given directory.
	 * 
	 * @param inFile
	 * @return
	 */
	public static List<File> listFiles(File inDir, boolean inTraversSubDirectories) {

		if (inDir == null || !inDir.isDirectory()) {

			return new ArrayList<File>(0);

		}

		final List<File> theFiles = new ArrayList<File>(0);

		for (final File aFile : defaultIfNull(inDir.listFiles(), EMPTY_FILES)) {

			if (aFile.isFile()) {

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

	public static void traversFiles(String inFile, IFileTraverser inFileTraverser) {

		traversFiles(inFile == null ? null : new File(inFile), inFileTraverser);
	}

	public static void traversFiles(final File inBaseFile, final IFileTraverser inFileTraverser) {

		if (inFileTraverser == null) {

			return;
		}

		class Helper {

			boolean isCancelled = false;
			Exception lastException = null;
			int dirCount = 0;
			int fileCount = 0;

			void doFileHandling(File inFile) {

				try {

					isCancelled = !inFileTraverser.handleFile(inFile);

					fileCount += isCancelled ? 0 : 1;

				} catch (final Exception e) {

					handleException(inFile, false, false, e);

				} finally {

					fileCount += isCancelled ? 0 : 1;
				}
			}

			void handleException(File inFile, boolean isStart, boolean isEnd, Exception inException) {

				lastException = inException;

				try {

					isCancelled = !inFileTraverser.handleException(inFile, false, false, lastException);

				} catch (final Exception e2) {
					// swallowed
					isCancelled = true;
				}
			}
		}

		final Helper theHelper = new Helper();

		try {

			theHelper.isCancelled = !inFileTraverser.start(inBaseFile);

		} catch (final Exception e) {

			theHelper.handleException(inBaseFile, true, false, e);
		}

		if (!theHelper.isCancelled && inBaseFile != null && inBaseFile.isFile()) {

			theHelper.doFileHandling(inBaseFile);
		}

		if (!theHelper.isCancelled && (inBaseFile == null || inBaseFile.isDirectory())) {

			final List<File> theDirQueue = new ArrayList<File>();

			if (inBaseFile != null) {

				theDirQueue.add(inBaseFile);

			} else {

				try {

					theHelper.isCancelled = !inFileTraverser.handleDirStart(inBaseFile);

				} catch (final Exception e) {

					theHelper.handleException(inBaseFile, false, false, e);
				}
			}

			// System.out.println("X: " + theDirQueue.size() + " looping...");

			while (!theHelper.isCancelled && !theDirQueue.isEmpty()) {

				final File theDir = theDirQueue.remove(0);

				// System.out.println("X: " + theDirQueue.size() + " rmv1st: " +
				// theDir);

				try {

					theHelper.isCancelled = !inFileTraverser.handleDirStart(theDir);

				} catch (final Exception e) {

					theHelper.handleException(inBaseFile, false, false, e);

				} finally {

					if (theHelper.isCancelled) {
						break;
					}
				}

				for (final File aChildFile : list(theDir)) {

					if (aChildFile.isDirectory()) {

						theDirQueue.add(aChildFile);
						// System.out.println("X: " + theDirQueue.size() + "
						// added: " + aChildFile);

					} else {

						theHelper.doFileHandling(aChildFile);

					}
				}

				try {

					theHelper.isCancelled = !inFileTraverser.handleDirEnd(theDir);

				} catch (final Exception e) {

					theHelper.handleException(theDir, false, false, e);

				} finally {

					theHelper.dirCount += theHelper.isCancelled ? 0 : 1;

					if (theHelper.isCancelled) {
						break;
					}
				}
			}

			if (inBaseFile == null) {

				try {

					theHelper.isCancelled = !inFileTraverser.handleDirEnd(inBaseFile);

				} catch (final Exception e) {

					theHelper.handleException(inBaseFile, false, false, e);
				}
			}
		}

		try {

			inFileTraverser.end(theHelper.dirCount, theHelper.fileCount, theHelper.isCancelled,
					theHelper.lastException);

		} catch (final Exception e) {

			theHelper.handleException(inBaseFile, false, true, e);
		}
	}

	/**
	 * NPE safe {@link String} startsWith
	 * 
	 * @param inStart
	 * @param inValue
	 * @return
	 */
	public static boolean startsWith(String inStart, String inValue) {

		if (inStart == null && inValue == null) {

			return true;

		} else if (inStart == null || inValue == null) {

			return false;
		}

		return inValue.startsWith(inStart);
	}

	/**
	 * NPE safe {@link String} startsWith
	 * 
	 * @param inStart
	 * @param inValue
	 * @return
	 */
	public static boolean endsWith(String inEnd, String inValue) {

		if (inEnd == null && inValue == null) {

			return true;

		} else if (inEnd == null || inValue == null) {

			return false;
		}

		return inValue.endsWith(inEnd);
	}

	/**
	 * @param inValue
	 * @return length is 0 or depending on setting a reaction <code>null</code>
	 *         value
	 */
	public static boolean isEmpty(String inValue, boolean isNullEmpty) {

		return inValue == null ? isNullEmpty : inValue.isEmpty();
	}

	public static boolean isBlank(String inValue) {
		return inValue == null || inValue.trim().isEmpty();
	}

	public static boolean isBlank(Object inValue, boolean inTostringNonString) {

		if (inValue == null) {

			return true;

		} else if (inValue instanceof String || inTostringNonString) {

			return isBlank(inValue.toString());

		}

		return false;
	}

	public static boolean isNotBlank(Object inValue, boolean inTostringNonString) {

		if (inValue == null) {

			return false;

		} else if (inValue instanceof String || inTostringNonString) {

			return isNotBlank(inValue.toString());
		}

		return false;
	}

	/**
	 * @param inValues
	 * @return <code>true</code> if all passed values are blank
	 */
	public static boolean isBlank(String... inValues) {

		if (inValues == null) {
			return true;
		}

		for (final String aValue : inValues) {

			if (isNotBlank(aValue)) {

				return false;
			}
		}

		return true;
	}

	/**
	 * @param inValues
	 * @return <code>true</code> if any passed value is blank
	 */
	public static boolean isAnyBlank(String... inValues) {

		if (inValues == null) {
			return true;
		}

		for (final String aValue : inValues) {

			if (isBlank(aValue)) {

				return true;
			}
		}

		return false;
	}
	
	public static boolean isNoneBlank(String... inValues) {
		
		if (inValues == null) {
			return false;
		}

		for (final String aValue : inValues) {

			if (isBlank(aValue)) {

				return false;
			}
		}

		return true;
	}

	public static boolean isNotBlank(String inValue) {
		return !isBlank(inValue);
	}

	public static String trim(String inValue, String inDefault) {

		inValue = inValue == null ? "" : inValue.trim();
		return !inValue.isEmpty() ? inValue : inDefault;
	}

	public static String trim(String inValue) {
		return inValue == null ? null : inValue.trim();
	}

	public static String trimToNull(String inValue) {
		return trim(inValue, null);
	}

	public static String trimToEmpty(String inValue) {
		return trim(inValue, "");
	}

	public static Properties loadProperties(File in) throws IOException {

		return loadProperties(
				in == null || !in.isFile() || !in.canRead() ? (InputStream) null : new FileInputStream(in));
	}

	public static Properties loadProperties(Properties inProperties, File in) throws IOException {

		return loadProperties(inProperties,
				in == null || !in.isFile() || !in.canRead() ? (InputStream) null : new FileInputStream(in));
	}

	public static Properties loadPropertiesQuietly(Properties inProperties, URL... inUrls) {

		return loadPropertiesQuietly(inProperties, inUrls == null ? null : Arrays.asList(inUrls));
	}

	@SuppressWarnings("unchecked")
	public static Properties loadPropertiesQuietly(Properties inProperties, Collection<URL> inUrls) {

		inUrls = inUrls == null ? EMPTY_LIST : inUrls;

		for (final URL anUrl : inUrls) {

			inProperties = loadPropertiesQuietly(inProperties, anUrl);
		}

		return inProperties;
	}

	public static Properties loadPropertiesQuietly(Properties inProperties, URL inUrl) {

		try {

			return loadProperties(inProperties, inUrl);

		} catch (final IOException e) {

			return inProperties != null ? inProperties : new Properties();
		}
	}

	public static Properties loadPropertiesQuietly(URL... inUrls) {

		final Properties theProperties = new Properties();

		each(new IHandler<URL>() {

			@Override
			public boolean handle(URL inUrl, int inIdx) {

				loadPropertiesQuietly(theProperties, inUrl);
				return true;
			}

			@Override
			public boolean handleException(int inIdx, Exception inException, URL inObject) {
				return true;
			}

			@Override
			public void done(int inHandles) {
			}

		}, inUrls);

		return theProperties;
	}

	public static Properties loadPropertiesQuietly(List<URL> inUrls) {

		final Properties theProperties = new Properties();

		each(new IHandler<URL>() {

			@Override
			public boolean handle(URL inUrl, int inIdx) {

				loadPropertiesQuietly(theProperties, inUrl);

				return true;
			}

			@Override
			public boolean handleException(int inIdx, Exception inException, URL inUrl) {
				return true;
			}

			@Override
			public void done(int inHandles) {

			}

		}, inUrls);

		return theProperties;
	}

	public static Properties loadPropertiesQuietly(URL inUrl) {

		final Properties p = new Properties();

		try {

			loadProperties(p, inUrl);

		} catch (final IOException e) {
		}

		return p;
	}

	public static Properties loadPropertiesQuietly(File in) {

		try {

			return loadProperties(new FileInputStream(in));

		} catch (final Exception e) {

			return new Properties();
		}
	}

	public static Properties loadPropertiesQuietly(InputStream in) {

		return loadPropertiesQuietly(null, in);
	}

	public static Properties loadPropertiesQuietly(Properties inProperties, InputStream in) {

		try {

			return loadProperties(inProperties, in);

		} catch (final Exception e) {

			return inProperties != null ? inProperties : new Properties();
		}
	}

	public static Properties loadProperties(URL inUrl) throws IOException {

		return loadProperties(null, inUrl);
	}

	public static Properties loadProperties(Properties inProperties, URL inUrl) throws IOException {

		final InputStream theIs = inUrl == null ? null : inUrl.openStream();

		try {

			return loadProperties(inProperties, theIs);

		} finally {

			close(theIs);
		}
	}

	public static Properties loadProperties(InputStream in) throws IOException {

		return loadProperties(null, in);
	}

	public static Properties loadProperties(Properties inProperties, InputStream in) throws IOException {

		final Properties theProperties = new Properties();

		try {

			theProperties.load(in);

			if (inProperties == null) {

				return theProperties;

			}

			inProperties.putAll(theProperties);
			return inProperties;

		} catch (final Exception e) {

			throw e instanceof IOException ? (IOException) e : new IOException(e);
		}
	}

	public static <T> Iterator<T> getEndlessLoopingIteration(@SuppressWarnings("unchecked") final T... inValues) {

		return new Iterator<T>() {

			Integer index = inValues == null ? null : -1;

			@Override
			public boolean hasNext() {
				return index != null && inValues.length > 0;
			}

			@Override
			public T next() {

				if (!hasNext()) {

					throw new UnsupportedOperationException("No items available");
				}

				return inValues[index = index == inValues.length ? 0 : index + 1];
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}

	public static <T> Iterator<T> getEndlessLoopingIteration(final Collection<T> inValues) {

		return new Iterator<T>() {

			Iterator<T> current = inValues == null ? null : inValues.iterator();

			@Override
			public boolean hasNext() {

				if (current == null) {
					return false;
				} else if (!current.hasNext()) {
					current = inValues.iterator();
				}
				return current.hasNext();
			}

			@Override
			public T next() {

				if (!hasNext()) {

					throw new UnsupportedOperationException("No items available");
				}

				return current.next();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}

	public static File getDirectoryPath(String inPath) throws IOException {

		final File thePath = inPath == null || inPath.trim().isEmpty() ? null : new File(inPath.trim());

		return thePath == null || !thePath.isDirectory() ? null : thePath.getCanonicalFile();

	}

	public static void mergeProperties(Properties inSrcProperties, Map<String, String> inTargetMap) {

		final Enumeration<Object> theKeys = inSrcProperties.keys();

		while (theKeys.hasMoreElements()) {

			final Object theKey = theKeys.nextElement();

			if (theKey instanceof String) {

				inTargetMap.put(theKey.toString(), inSrcProperties.getProperty(theKey.toString()));
			}
		}
	}

	public static void close(Closeable inCloseable) {
		close(inCloseable, false);
	}

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

	public static void close(Closeable inCloseable, boolean inFlush) {

		if (inFlush) {
			flush(inCloseable);
		}

		try {

			inCloseable.close();

		} catch (final Exception e) {
		}

	}

	public static void close(Closeable... inCloseable) {

		if (inCloseable != null) {

			for (final Closeable closeable : inCloseable) {

				close(closeable);
			}
		}
	}

	public static void close(Collection<Closeable> inCloseables) {

		if (inCloseables != null) {

			for (final Closeable closeable : inCloseables) {

				close(closeable);
			}
		}
	}

	public static InputStream createInputStreamQuietly(File inFile) {

		try {

			return new FileInputStream(inFile);

		} catch (final Exception e) {

			// swallow
		}

		return null;
	}

	public static List<String> split(String inString, char inSeparatorChar) {

		return split(inString, inSeparatorChar, false, false);
	}

	public static String getSplitPart(String inString, char inSeparatorChar, int inIndex) {

		try {

			final List<String> theSplits = split(inString, inSeparatorChar);

			return theSplits.get(inIndex >= 0 ? inIndex : theSplits.size() + inIndex);

		} catch (final Exception e) {
			return null;
		}
	}

	/**
	 * TODO: document clearly or rewrite with proper regex
	 * 
	 * @param inString
	 * @param inSeparatorChar
	 * @return
	 */
	public static List<String> split(String inString, char inSeparatorChar, boolean inTrimSplits,
			boolean inRemoveBlanks) {

		final List<String> theValues = new ArrayList<String>(1);

		if (inString == null) {
			return theValues;
		}

		final StringBuilder sb = new StringBuilder();

		for (int i = 0, len = inString.length(); i < len; i++) {

			final char theChar = inString.charAt(i);

			if (theChar == inSeparatorChar) {

				final String theValue = inTrimSplits ? sb.toString().trim() : sb.toString();
				sb.setLength(0);

				if (!inRemoveBlanks || !theValue.isEmpty()) {

					theValues.add(theValue);
				}

			} else {

				sb.append(theChar);
			}
		}

		final String theValue = sb.toString().trim();
		sb.setLength(0);

		if (!theValue.isEmpty()) {

			theValues.add(theValue);
		}

		return theValues;
	}

	public static InputStream createUrlInputStream(String inUrl) throws IOException {

		return new URL(inUrl).openStream();
	}

	public static String getCanonicalPath(File inFile, boolean inThrowRuntimeException) {

		try {
			return inFile.getCanonicalPath();
		} catch (final IOException e) {

			if (inThrowRuntimeException) {
				throw new RuntimeException(inFile == null ? "File is null" : e.getMessage(), e);
			}

			return null;
		}
	}

	public static File getCanonicalPathQuietly(String inPath) {

		return getCanonicalPathQuietly(inPath, false);
	}

	public static File getCanonicalPathQuietly(String inPath, boolean inReturnAbsolutPathInstead) {

		return inPath == null ? null : getCanonicalPathQuietly(new File(inPath), inReturnAbsolutPathInstead);
	}

	public static File getCanonicalPathQuietlyOrAbsoultePath(File inFile) {
		return getCanonicalPathQuietly(inFile, true);
	}

	public static File getCanonicalPathQuietly(File inFile) {
		return getCanonicalPathQuietly(inFile, false);
	}

	public static String getCanonicalPathStringQuietly(File inFile, boolean inReturnAbsolutPathInstead) {

		final File thePath = getCanonicalPathQuietly(inFile, inReturnAbsolutPathInstead);
		return thePath == null ? null : thePath.getAbsolutePath();
	}

	public static File getCanonicalPathQuietly(File inFile, boolean inReturnAbsolutPathInstead) {

		try {

			return inFile.getCanonicalFile();

		} catch (final Exception e) {

			try {

				return inReturnAbsolutPathInstead ? inFile.getAbsoluteFile() : null;

			} catch (final Exception e2) {
			}
		}

		return null;
	}

	public static String getMd5(File inValue) {

		if (inValue == null || !inValue.exists()) {
			return null;
		} else if (inValue.isDirectory()) {
			throw new IllegalArgumentException("file is directory: " + getCanonicalPathStringQuietly(inValue, true));
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

			throw new IllegalArgumentException("file is no file: " + getCanonicalPathQuietly(inFile, true));
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

	private static Wrapper<Long> getOrCreateCounter(String inCounter) {

		Wrapper<Long> c = COUNTERS.get(inCounter);

		if (c == null) {

			synchronized (COUNTERS) {

				c = COUNTERS.get(inCounter);
				COUNTERS.put(inCounter, c = (c != null ? c : new Wrapper<Long>(0L)));
			}
		}

		return c;
	}

	public static long getCounter(String inCounter) {

		return getCounter(inCounter, null);
	}

	public static long getCounter(String inCounter, Long inIncrease) {

		final Wrapper<Long> c = getOrCreateCounter(inCounter);

		synchronized (c) {

			if (inIncrease != null) {
				c.value += inIncrease;
			}

		}

		return c.value;
	}

	public static void loadCpResourceLines(String inResource, String inEncoding, ILineHandler inLineHandler)
			throws Exception {

		final InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(inResource);

		try {

			loadLines(is, inEncoding, inLineHandler);

		} finally {

			close(is);
		}
	}

	public static void loadFileLines(String inFile, String inEncoding, ILineHandler inLineHandler) throws Exception {

		loadLines(new File(inFile), inEncoding, inLineHandler);
	}

	public static void loadLines(String inUrl, String inEncoding, ILineHandler inLineHandler) throws Exception {

		loadLines(inUrl == null ? null : new URL(inUrl), inEncoding, inLineHandler);
	}

	public static void loadLines(File inFile, String inEncoding, ILineHandler inLineHandler) throws Exception {

		loadLines(inFile == null ? null : inFile.toURI().toURL(), inEncoding, inLineHandler);

	}

	public static void loadLines(URL inUrl, String inEncoding, ILineHandler inLineHandler) throws Exception {

		final InputStream is = inUrl == null ? null : inUrl.openStream();

		try {

			loadLines(is, inEncoding, inLineHandler);

		} finally {

			close(is);
		}
	}

	public static void loadLines(InputStream inIs, String inEncoding, ILineHandler inLineHandler) throws Exception {

		inEncoding = Utils.trimToNull(inEncoding);
		final BufferedReader theIn = new BufferedReader(new InputStreamReader(inIs,
				inEncoding == null ? Charset.defaultCharset() : Charset.forName(inEncoding)));

		int theLineCount = 0;

		while (theIn.ready()) {

			inLineHandler.handle(++theLineCount, theIn.readLine());
		}

		inLineHandler.done(theLineCount);
	}

	@SafeVarargs
	public static <T> Set<T> toSet(T... inValues) {
		return toSet(true, inValues);
	}

	@SafeVarargs
	public static <T> Set<T> toSet(boolean inIncludeNull, T... inValues) {

		if (inValues == null) {
			return null;
		}

		final Set<T> s = new HashSet<T>(inValues.length);

		for (final T aValue : inValues) {

			if (aValue == null && !inIncludeNull) {
				continue;
			}

			s.add(aValue);
		}

		return s;
	}

	@SafeVarargs
	public static <T> Set<T> intersectSets(Set<T>... inSets) {

		final Set<T> theResultSet = new HashSet<T>();

		if (inSets == null) {
			return theResultSet;
		}

		boolean isFirst = true;

		for (final Set<T> aSet : inSets) {

			if (aSet == null) {
				return new HashSet<T>();
			} else if (isFirst) {
				theResultSet.addAll(aSet);
				isFirst = false;
			} else {

				for (final T anExistingItem : new HashSet<T>(theResultSet)) {

					if (!aSet.contains(anExistingItem)) {

						theResultSet.remove(anExistingItem);
					}
				}
			}
		}

		return theResultSet;
	}

	public static <T> Iterable<T> getReversedListIterable(final List<T> inList) {

		return new Iterable<T>() {
			@Override
			public Iterator<T> iterator() {
				return getReversedListIterator(inList);
			}
		};
	}

	public static <T> Iterator<T> getReversedListIterator(final List<T> inList) {

		return new Iterator<T>() {

			int index = inList == null ? -1 : inList.size() - 1;

			@Override
			public boolean hasNext() {
				return index >= 0;
			}

			@Override
			public T next() {

				return inList.get(index--);
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	public static boolean isFolder(String inPath) {

		return isFolder(inPath == null ? null : new File(inPath));
	}

	public static boolean isFolder(File inPath) {

		return inPath != null && inPath.isDirectory();
	}

	public static boolean isDirectory(String inPath) {

		return isFolder(inPath == null ? null : new File(inPath));
	}

	public static boolean isDirectory(File inPath) {

		return inPath != null && inPath.isDirectory();
	}

	public static boolean isFile(String inPath) {
		return isNotBlank(inPath) && isFile(new File(inPath));
	}

	public static boolean isFile(File file) {
		return file != null && file.isFile();
	}

	public static int getRandom(int inMin, int inMax) {

		inMax++;

		if (inMax < inMin) {
			final int t = inMax;
			inMax = inMin;
			inMin = t;
		}

		// System.out.println("iMn:" + inMin + ", iMx: " + inMax + " ; 0.." +
		// (inMax + inMin) + " :: " + (0 + inMin) + ".." + (inMax + inMin));

		return RANDOM.nextInt(inMax - inMin) + inMin;
	}

	public static <T> T getRandom(boolean inAllowNull, @SuppressWarnings("unchecked") T... inVals) {

		final int r = getRandom(inAllowNull ? -1 : 0, inVals.length - 1);
		return r == -1 ? null : inVals[r];
	}

	public static String toStringSafe(Object inObject) {
		return toStringSafe(inObject, null);
	}

	public static String toStringSafe(Object inObject, String inDefault) {
		return inObject == null ? inDefault : inObject.toString();
	}

	public static String toString(Object inObject) {

		return toString(inObject, null);
	}

	public static String toString(Object inObject, Integer inMaxLength) {

		if (inObject instanceof Exception) {

			final Exception e = (Exception) inObject;
			inObject = e.getClass().getName() + ":" + e.getMessage();

		} else if (inObject != null && inObject.getClass().isArray()) {

			final Object[] theArray = new Object[Array.getLength(inObject)];

			for (int i = 0; i < theArray.length; i++) {

				theArray[i] = toString(Array.get(inObject, i), null);
			}

			inObject = "<" + inObject + ":" + theArray.length + ">" + Arrays.toString(theArray);

		} else if (inObject instanceof File) {

			final File theFile = (File) inObject;

			inObject = getCanonicalPathQuietly(theFile, true);

			if (theFile.exists() && theFile.isFile()) {

				inObject += (" [file, size=" + Utils.toStringDataSize(theFile.length()) + ", modified="
						+ Timer.format(Timer.DATE_YYYY_MM_DD_HH_MM_SS_SSS, theFile.lastModified()) + "]");

			} else if (theFile.exists() && theFile.isFile()) {

				inObject += (" [directory, modified="
						+ Timer.format(Timer.DATE_YYYY_MM_DD_HH_MM_SS_SSS, theFile.lastModified()) + "]");
			}
		} else if (inObject instanceof Method) {

			final Method theMethod = ((Method) inObject);

			final StringBuilder theString = new StringBuilder();

			theString.append(theMethod.getDeclaringClass().getName());
			theString.append(".");
			theString.append(theMethod.getName());
			theString.append('(');
			boolean isFirst = true;
			for (final Class<?> anArgClass : theMethod.getParameterTypes()) {

				if (!isFirst) {
					theString.append(", ");
					isFirst = false;
				}

				theString.append(anArgClass.getName());
			}

			theString.append(')');

			inObject = theString.toString();
			theString.setLength(0);
		} else if (inObject instanceof Document) {

			final TransformerFactory tf = TransformerFactory.newInstance();
			try {

				final Transformer transformer = tf.newTransformer();
				transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
				transformer.setOutputProperty(OutputKeys.METHOD, "xml");
				transformer.setOutputProperty(OutputKeys.INDENT, "yes");
				transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
				transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");

				final ByteArrayOutputStream theBaso = new ByteArrayOutputStream();

				transformer.transform(new DOMSource((Node) inObject), new StreamResult(theBaso));

				return new String(theBaso.toByteArray());

			} catch (Exception e) {

				return inObject.toString();
			}

		}

		String theResult = String.valueOf(inObject);

		if (inMaxLength != null && inMaxLength > 0) {

			final int theOriginalLen = theResult.length();
			theResult = theResult.substring(0, Math.min(theOriginalLen, inMaxLength));

			if (theResult.length() < theOriginalLen) {

				theResult += "... (" + (theResult.length() - inMaxLength) + ")";
			}
		}

		return theResult;
	}

	public static int byteToPositiveInt(byte inByte) {
		return inByte + 128;
	}

	public static byte positiveIntToByte(int inInt) {
		return (byte) (inInt - 128);
	}

	public static long getCurrentMillis() {
		return Timer.getMillis();
	}

	public static <T> T defaultIfNull(T inValue, T inDefault) {

		return inValue == null ? inDefault : inValue;
	}

	public static String defaultIfBlank(String inValue, String inDefault) {

		return isBlank(inValue) ? inDefault : inValue;
	}

	public static String defaultIfUnmatch(String inValue, Pattern inPattern, String inDefault) {

		return isMatch(inValue, inPattern) ? inValue : inDefault;
	}

	public static String defaultIfUnmatch(String inValue, String inPattern, String inDefault) {

		return isMatch(inValue, inPattern) ? inValue : inDefault;
	}

	public static int nthIndexOf(String inString, char c, int inNth) {

		inString = inString == null || inNth < 1 ? "" : inString;

		for (int i = 0; i < inString.length(); i++) {

			if (inString.charAt(i) == c && --inNth == 0) {
				return i;
			}
		}

		return -1;
	}

	public static <T> List<T> intersect(List<T> in1, List<T> in2) {

		final List<T> theResults = new ArrayList<T>(0);

		if (in1 == null || in2 == null || in1.isEmpty() || in2.isEmpty()) {

			return theResults;
		}

		for (final T anObject : in1) {

			if (anObject == null && in2.contains(null)) {

				theResults.add(anObject);

			} else if (anObject != null && in2.contains(anObject)) {

				theResults.add(anObject);
			}
		}

		return theResults;
	}

	public static <T> List<T> intersect(List<T> in1, T[] in2) {

		final List<T> theResults = new ArrayList<T>(0);

		if (in1 == null || in2 == null || in1.isEmpty() || in2.length == 0) {

			return theResults;
		}

		for (final T anObject : in1) {

			if (isContaining(in2, anObject)) {

				theResults.add(anObject);
			}
		}

		return theResults;
	}

	public static <T> List<T> intersect(T[] in1, List<T> in2) {

		return intersect(in2, in1);
	}

	public static <T> boolean isContainingNull(T[] inValues) {

		return isContaining(inValues, null);
	}

	public static <T> boolean isContaining(T[] inValues, T inValue) {

		if (inValues == null) {
			return false;
		}

		for (final T aValue : inValues) {

			if (aValue == inValue) {
				return true;
			} else if (aValue != null && aValue.equals(inValue)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * check if the resource path is a subpath of the basepath
	 * @param inBasePath
	 * @param inResourcePath
	 * @return
	 */
	public static boolean isLegalFilePath(File inBasePath, File inResourcePath) {

		try {

			return inResourcePath.getCanonicalPath().startsWith(inBasePath.getCanonicalPath());

		} catch (final Exception e) {

			return false;
		}
	}

	public static void serializeObject(File inFile, Serializable inObject) throws IOException {
		serializeObject(new FileOutputStream(inFile), inObject, true);
	}

	public static <T> T deserializeObject(File inFile) throws ClassNotFoundException, IOException {

		return deserializeObject(new FileInputStream(inFile), true);
	}

	public static void serializeObject(OutputStream inOutputStream, Serializable inObject, boolean inCloseStream)
			throws IOException {

		if (inObject == null) {
			return;
		}

		ObjectOutputStream theOs = null;

		try {

			theOs = new ObjectOutputStream(inOutputStream);
			theOs.writeObject(inObject);
			theOs.flush();

		} finally {

			if (inCloseStream) {

				close(theOs);
			}
		}

	}

	@SuppressWarnings("unchecked")
	public static <T> T deserializeObject(InputStream inInputStream, boolean inCloseStream)
			throws IOException, ClassNotFoundException {

		ObjectInputStream theIs = null;

		try {

			theIs = new ObjectInputStream(inInputStream);
			return (T) theIs.readObject();

		} finally {

			if (inCloseStream) {

				close(theIs);
			}
		}

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

	public static String getFullStacktrace(Throwable inException) {

		return getFullStacktrace(inException, null);
	}

	public static String getFullStacktrace(Throwable inException, String inRegex) {

		if (inException == null) {
			return null;
		}

		final IdentitySet<Throwable> thePrintedThrowables = new IdentitySet<Throwable>(2);

		final Pattern theRegex = compileRegex(inRegex);

		final StringBuilder sb = new StringBuilder();

		while (inException != null) {

			thePrintedThrowables.add(inException);

			sb.append(Utils.toString(inException, null)).append("\n");

			final StackTraceElement[] theStes = inException.getStackTrace();

			for (int i = 0; i < theStes.length; i++) {

				if (theRegex == null || isMatch(theStes[i].toString(), theRegex)) {

					sb.append(i + 1).append(" ").append(theStes[i]).append("\n");
				}
			}

			inException = inException.getCause();

			if (thePrintedThrowables.contains(inException)) {

				break;
			}
		}

		return sb.toString().trim();
	}

	public static Pattern compileRegex(String inRegex) {

		try {

			return inRegex == null ? null : Pattern.compile(inRegex);

		} catch (final Exception e) {

			return null;
		}
	}

	public static <T> Iterable<T> toIterable(final Enumeration<T> inEnum) {

		return new Iterable<T>() {

			@Override
			public Iterator<T> iterator() {

				return toIterator(inEnum);
			}
		};
	}

	public static <T> Iterator<T> toIterator(final Enumeration<T> inEnum) {

		return new Iterator<T>() {

			@Override
			public boolean hasNext() {
				return inEnum != null && inEnum.hasMoreElements();
			}

			@Override
			public T next() {

				if (inEnum == null) {

					throw new UnsupportedOperationException();
				}

				return inEnum.nextElement();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}

	public static void readJson(File inFile, IJsonEventParser inParser) throws IOException {

		if (inFile == null || inParser == null) {
			return;
		}

		class Pp {

			Pp parent;
			Integer index;
			String name;

			Pp(Pp p) {

				parent = p;
			}

			String getPath() {

				String p = "";
				Pp cp = this;

				while (cp != null) {

					if (cp.name != null) {
						p = "/" + cp.name + p;
					} else if (cp.index != null) {
						p = "[" + cp.index + "]" + p;
						// } else {
						// p = "/" + p;
					}

					cp = cp.parent;
				}

				return p;
			}

			public String getSimplePath() {

				String p = "";
				Pp cp = this;

				while (cp != null) {

					if (cp.name != null) {
						p = "/" + cp.name + p;
					} else if (cp.index != null) {
						// swallow
						// } else {
						// p = "/" + p;
					}

					cp = cp.parent;
				}

				return p;
			}

			public String nameBefore() {
				return parent == null ? null : parent.name;
			}

			public Integer indexBefore() {
				return parent == null ? null : parent.index;
			}
		}

		final JsonFactory jsonFactory = new JsonFactory();
		final JsonParser jp = jsonFactory.createParser(inFile);
		jp.enable(Feature.ALLOW_COMMENTS);
		jp.enable(Feature.ALLOW_UNQUOTED_FIELD_NAMES);

		JsonToken t = jp.nextToken();

		Pp pp = null;

		inParser.documentStart();

		while (!jp.isClosed() && jp.hasCurrentToken()) {

			switch (t) {

			case START_OBJECT:

				// System.out.println("\t{-start");

				if (pp != null && pp.index != null) {
					pp.index++;
				}
				pp = new Pp(pp);
				inParser.objectStart(pp.getPath(), pp.getSimplePath(), pp.nameBefore(), pp.indexBefore());
				break;

			case END_OBJECT:
				// System.out.println("\tend-}");
				pp.name = null;
				inParser.objectEnd(pp.getPath(), pp.getSimplePath(), pp.nameBefore(), pp.indexBefore());
				pp = pp.parent;
				break;

			case START_ARRAY:
				// System.out.println("\t[-start");
				if (pp != null && pp.index != null) {
					pp.index++;
				}
				pp = new Pp(pp);
				inParser.arrayStart(pp.getPath(), pp.getSimplePath(), pp.nameBefore(), pp.indexBefore());
				pp.index = -1;
				break;

			case END_ARRAY:
				// System.out.println("\tend-]");
				pp.index = null;
				inParser.arrayEnd(pp.getPath(), pp.getSimplePath(), pp.nameBefore(), pp.indexBefore());
				pp = pp.parent;
				break;

			case FIELD_NAME:
				// System.out.println("\tn=" + jp.getCurrentName());
				pp.name = jp.getCurrentName();
				break;

			case VALUE_NULL:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handleNull(pp.getPath(), pp.getSimplePath(), pp.name, pp.index);

			case VALUE_STRING:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handle(pp.getPath(), pp.getSimplePath(), pp.name, pp.index, jp.getText());
				break;

			case VALUE_TRUE:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handle(pp.getPath(), pp.getSimplePath(), pp.name, pp.index, true);
				break;

			case VALUE_FALSE:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handle(pp.getPath(), pp.getSimplePath(), pp.name, pp.index, false);
				break;

			case VALUE_NUMBER_INT:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handle(pp.getPath(), pp.getSimplePath(), pp.name, pp.index,
						new BigDecimal(jp.getValueAsString()));
				break;

			case VALUE_NUMBER_FLOAT:

				pp.index = pp.index == null ? null : (pp.index + 1);
				inParser.handle(pp.getPath(), pp.getSimplePath(), pp.name, pp.index,
						new BigDecimal(jp.getValueAsString()));
				break;

			default:
				throw new IllegalStateException("must never happen. no handling on " + t);
			}

			t = jp.nextToken();
		}

		inParser.documentEnd();
	}

	public static String readFileQuietly(String inFile, String inCharset) {

		return readFileQuietly(inFile, inCharset == null ? Charset.defaultCharset() : Charset.forName(inCharset));
	}

	public static String readFileQuietly(String inFile, Charset inCharset) {

		try {

			return readFile(new File(inFile), inCharset);

		} catch (final Exception e) {
			// swallow
			return null;
		}
	}

	public static String readFileQuietly(File inFile, Charset inCharset) {

		try {

			return readFile(inFile, inCharset);

		} catch (final Exception e) {
			// swallow
			return null;
		}
	}

	public static String readString(InputStream inIs) throws IOException {
		return readString(inIs, null);
	}

	public static String readString(InputStream inIs, Charset inCharset) throws IOException {
		return new String(readBytes(inIs, true), inCharset == null ? Charset.defaultCharset() : inCharset);
	}

	public static byte[] readBytes(InputStream inIs, boolean inClose) throws IOException {

		final ByteArrayOutputStream theBaos = new ByteArrayOutputStream();

		try {

			copy(inIs, theBaos);

		} finally {

			if (inClose) {

				close(inIs);
			}
		}

		return theBaos.toByteArray();
	}

	public static byte[] readBytes(File inFile) throws IOException {

		if (inFile == null || !inFile.isFile()) {
			return null;
		}

		return readBytes(new FileInputStream(inFile), true);
	}

	public static byte[] loadFile(File inFile) throws IOException {
		
		return readBytes( inFile );
	}
	
	public static byte[] loadFileSafe(File inFile, byte[] inDefault) {
		
		try {

			return loadFile(inFile);
			
		} catch (Exception e) {
			// swallow
			return inDefault;
		}
	}
	
	public static byte[] loadFileSafe(File inFile) {
		
		return loadFileSafe(inFile, null);
	}

	public static String readFile(File inFile, Charset inCharset) throws IOException {

		return new String(readBytes(new FileInputStream(inFile), true),
				inCharset == null ? Charset.defaultCharset() : inCharset);
	}

	public static String readFile(URL inUrl) throws IOException {

		final InputStream theIs = inUrl.openStream();

		try {

			return readFile(theIs);

		} finally {
			close(theIs);
		}

	}

	public static String readFile(InputStream inInputStream) throws IOException {

		return readFile(inInputStream, null);
	}

	public static String readFile(InputStream inInputStream, Charset inCharset) throws IOException {

		return readFile(new InputStreamReader(inInputStream, defaultIfNull(inCharset, Charset.defaultCharset())));
	}

	public static String readFileToString(String inFile) throws IOException {

		return readFile(inFile == null ? null : new File(inFile));
	}
	
	public static String readFileLine(File inFile, int inLineIndex, Charset inCharset) throws IOException {
		
		if ( !isFile( inFile ) || inLineIndex < 0) { return null; }
		
		BufferedReader r = null;
		
		try {
		
			int lineCnt = -1;
			
			r = new BufferedReader( new InputStreamReader( new FileInputStream(inFile) ,  inCharset == null ? Charset.defaultCharset() : inCharset ));
			
			while( r.ready() ) {
				
				String theLine = r.readLine();
				lineCnt++;
				
				if ( lineCnt == inLineIndex ) {
					
					return theLine;
				}
			}
			
		
		} finally {
			
			close( r );
		}
		
		return null;
	}

	public static String readFile(File inFile) throws IOException {

		return readFile(inFile, Charset.defaultCharset());
	}

	public static String readFile(File inFile, String inCharset) throws IOException {

		return readFile(inFile, inCharset == null ? Charset.defaultCharset() : Charset.forName(inCharset));
	}

	public static void writeFile(String inFile, String inString, String inCharset) throws IOException {

		writeFile(new File(inFile), inString, inCharset);
	}
	
	public static List<String> readLines(File inFile) throws IOException {
	
		return readFileLines(inFile);
	}
	
	public static List<String> readLines(File inFile, Charset inCharset) throws IOException {
		
		return readFileLines(inFile, inCharset);
	}
	
	public static List<String> readFileLines(File inFile) throws IOException {

		return readFileLines(inFile, Charset.defaultCharset());
	}
	
	public static List<String> readFileLines(File inFile, Charset inCharset) throws IOException {
	
		final List<String> theLines = new ArrayList<String>();
		
		final String lines = readFile(inFile, inCharset);
		
		if ( lines == null ) { return theLines; }
		
		for (String aLine : lines.split( "\n" , -1) ) {
			theLines.add( aLine );
		}
		
		
		return theLines;
	}
	
	public static void writeFile(File inFile, String inString) throws IOException {
		writeFile(inFile, inString, Charset.defaultCharset());
	}
	public static void writeFile(File inFile, String inString, String inCharset) throws IOException {

		writeFile(inFile, inString, inCharset == null ? null : Charset.forName(inCharset));
	}

	public static void writeFile(File inFile, String inString, Charset inCharset) throws IOException {

		writeFile(inFile,
				inString == null ? null : inString.getBytes(inCharset == null ? Charset.defaultCharset() : inCharset));
	}
	
	public static void writeFile(File inFile, String inString, Charset inCharset, boolean inAppend) throws IOException {
		
		writeFile(inFile,
				inString == null ? null : inString.getBytes(inCharset == null ? Charset.defaultCharset() : inCharset), inAppend);
	}
	
	public static void writeFile(File inFile, byte[] inBytes) throws IOException {
	
		writeFile( inFile, inBytes, false );
	}
	
	public static void writeFile(File inFile, byte[] inBytes, boolean inAppend) throws IOException {

		if (inBytes == null) {

			return;

		} else if (inFile == null) {

			throw new IOException("File is null");

		} else if ((inFile = inFile.getCanonicalFile()).isDirectory()) {

			throw new IOException("File is directory: "+ inFile);

		} else if (!inFile.getParentFile().exists()) {

			inFile.getParentFile().mkdirs();
		}

		if (!inFile.getParentFile().isDirectory()) {

			throw new IOException("Parent directory is not accessible: " + inFile.getParent());
		}

		final FileOutputStream theFos = new FileOutputStream(inFile, inAppend );
		
		try {

			theFos.write(inBytes);
			theFos.flush();

		} finally {

			close(theFos);
		}
	}

	public static long writeFile(File inFile, InputStream inInputStream) throws IOException {

		return writeFile(inFile, inInputStream, true);
	}

	public static long writeFile(File inFile, InputStream inInputStream, boolean inCloseStream) throws IOException {

		if (inFile != null && !inFile.getParentFile().exists()) {

			inFile.getParentFile().mkdirs();
		}

		return copy(inInputStream, new FileOutputStream(inFile), true);
	}

	public static void delete(File... inFiles) {

		if (inFiles == null) {
			return;
		}

		delete(Arrays.asList(inFiles));
	}

	/**
	 * Will return the given file as parameter if the file is not a directory.
	 * <br/>
	 * Traversing the file tree will always try to go down to the first deepest
	 * reachable file which is not a directory
	 * 
	 * @param inFlat
	 *            <code>true</code> if only the files within this directory will
	 *            be listed or <code>false</code> if it should travers down.
	 * @return
	 */
	public static List<File> listFiles___(File inDir, boolean inFlat) {

		if (inFlat) {

			return list(inDir);

		}

		final List<File> theFiles = new ArrayList<File>();

		if (inDir == null) {

			return theFiles;
		}

		traversFiles(inDir, new Traverser() {

			@Override
			public boolean handleFile(File inFile) throws Exception {

				theFiles.add(inFile);
				return true;
			}

			@Override
			public boolean handleDirStart(File inFile) throws Exception {

				theFiles.add(inFile);
				return true;
			}
		});

		return theFiles;
	}

	public static boolean delete(Collection<File> inFiles) {

		return delete(inFiles, false);
	}

	public static boolean delete(Collection<File> inFiles, final boolean inContinueOnExceptionOrFailure) {

		if (inFiles == null) {

			return true;
		}

		final Wrapper<Boolean> theResult = new Wrapper<Boolean>(true);

		for (final File aFile : inFiles) {

			if (aFile == null) {
				continue;
			}

			final List<File> theDirs = new ArrayList<File>(inFiles);

			traversFiles(aFile, new Traverser() {

				@Override
				public boolean handleFile(File inFile) throws Exception {

					if (inFile != null && inFile.exists()) {

						inFile.delete();

						if (inFile.exists()) {

							LOG.warn("File not deleted: " + inFile);
							theResult.value = false;

							if (!inContinueOnExceptionOrFailure) {

								return false;
							}
						}
					}

					return true;
				}

				@Override
				public boolean handleDirStart(File inFile) throws Exception {

					theDirs.add(0, inFile);
					return true;
				}

				@Override
				public boolean handleException(File inFile, boolean isStart, boolean isEnd, Exception inException) {
					theResult.value = false;
					return inContinueOnExceptionOrFailure;
				}
			});

			if (theResult.value == false && !inContinueOnExceptionOrFailure) {

				return false;
			}

			while (!theDirs.isEmpty()) {

				final File theDir = theDirs.remove(0);
				theDir.delete();

				if (theDir.exists()) {

					theResult.value = false;
					LOG.warn("Directory not deleted: " + theDir);

					if (!inContinueOnExceptionOrFailure) {

						return false;
					}
				}
			}
		}

		return theResult.value;
	}

	public static int[] toPositiveIntArray(byte[] inBytes) {

		if (inBytes == null) {
			return null;
		}

		final int[] theInts = new int[inBytes.length];

		for (int i = 0; i < theInts.length; i++) {

			theInts[i] = byteToPositiveInt(inBytes[i]);
		}

		return theInts;
	}

	public static byte[] toByteArrayFromPositiveInts(int[] inInts) {

		if (inInts == null) {
			return null;
		}

		final byte[] theBytes = new byte[inInts.length];

		for (int i = 0; i < theBytes.length; i++) {

			theBytes[i] = positiveIntToByte(inInts[i]);
		}

		return theBytes;
	}

	public static String toStringLines(Collection<?> inValues) {

		if (inValues == null) {
			return "";
		}

		final StringBuilder theBs = new StringBuilder();

		for (final Object object : inValues) {
			theBs.append(toString(object)).append("\n");
		}

		return theBs.toString().trim();
	}

	public static String toStringLines(Map<?, ?> inValues) {

		if (inValues == null) {
			return "";
		}

		final StringBuilder theBs = new StringBuilder();

		for (final Entry<?, ?> e : inValues.entrySet()) {
			theBs.append(toString(e.getKey())).append(" = ").append(e.getValue()).append("\n");
		}

		return theBs.toString().trim();
	}

	public static <T> List<T> toArray(Enumeration<T> inEnum) {

		if (inEnum == null) {
			return null;
		}

		final List<T> r = new ArrayList<T>();

		while (inEnum.hasMoreElements()) {

			r.add(inEnum.nextElement());
		}

		return r;
	}

	/**
	 * Exception free compiling of patterns
	 * 
	 * @param inRegex
	 * @return
	 */
	public static Pattern compilePattern(String inRegex) {

		try {

			return Pattern.compile(inRegex);

		} catch (final Exception e) {
			return null;
		}
	}

	public static boolean isPatternCompilable(String inRegex) {

		try {

			Pattern.compile(inRegex);
			return true;

		} catch (final Exception e) {

			return false;
		}
	}

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

	public static boolean isMatchAll(Pattern inPattern, String... inValues) {

		if (inValues != null) {

			for (String aValue : inValues) {

				if (isNoMatch(aValue, inPattern)) {
					return false;
				}
			}
		}

		return true;
	}

	public static boolean mkParentDirs(File inTarget) {

		if (inTarget == null) {

			return false;

		} else if (inTarget.exists()) {

			return true;
		}

		return inTarget.getParentFile().mkdirs();
	}

	public static String toStringTime(Number inNumber) {

		if (inNumber == null) {

			return "0";
		}

		final BigDecimal theNumber = (BigDecimal) (inNumber instanceof BigDecimal ? inNumber
				: new BigDecimal(inNumber.toString()));

		return Timer.getReadableDaysHoursMinutesSeconds(theNumber.longValue());
	}

	public static String getReadableDataSize(Long inSize) {
		return toStringDataSize(inSize);
	}

	public static String getReadableTime(Long inMillis) {

		if (inMillis == null) {
			return null;
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

	public static String toStringDataSize(Long inSize) {

		if (inSize == null) {

			return null;

		} else if (inSize < 1) {

			return inSize.toString();
		}

		if (inSize < 1024) {

			return inSize + " bytes";
		}

		BigDecimal theSize = new BigDecimal(inSize).divide(Utils.BD_1024, 1, RoundingMode.HALF_UP);

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

	/**
	 * @param inValue
	 * @return <code>true</code> if {@link BigDecimal} can build a number from
	 *         the value, <code>false</code> otherwise
	 */
	public static boolean isNumber(String inValue) {

		try {

			return new BigDecimal(inValue) != null;

		} catch (final Exception e) {

			return false;
		}
	}

	public static boolean isField(Field inField, Class<?> inType, Boolean isStatic, Boolean isFinal,
			Boolean isPrivate) {

		if (inField == null) {
			return false;

		} else if (inType != null && !inField.getType().isAssignableFrom(inType)) {

			return false;

		}

		final int m = inField.getModifiers();

		if (isStatic != null && Modifier.isStatic(m) != isStatic) {

			return false;

		} else if (isFinal != null && Modifier.isFinal(m) != isFinal) {

			return false;

		} else if (isPrivate != null && Modifier.isPrivate(m) != isPrivate) {

			return false;
		}

		return true;
	}

	/**
	 * Reads all the input and caches it in a byte array
	 * 
	 * @param inIs
	 * @return
	 * @throws IOException
	 */
	public static InputStream cacheStream(InputStream inIs) throws IOException {

		if (inIs == null) {
			return new ByteArrayInputStream(EMPTY_BYTES);
		}

		final ByteArrayOutputStream theBaos = new ByteArrayOutputStream();
		copy(inIs, theBaos);
		return new ByteArrayInputStream(theBaos.toByteArray());
	}

	/**
	 * @param inString
	 * @param inStrict
	 *            a previous match has to be found in order to apply the next
	 *            regex
	 * @param inRegexs
	 * @return
	 */
	public static List<String> extractPatterns(String inString, boolean inStrict, String... inRegexs) {

		final List<String> theResults = new ArrayList<String>(inRegexs == null ? 0 : inRegexs.length);

		if (Utils.isBlank(inString) || isBlank(inRegexs)) {

			while (inRegexs != null && theResults.size() != inRegexs.length) {

				theResults.add(null);
			}

			return theResults;
		}

		int lastIndex = 0;
		boolean wasNotFound = false;

		for (final String aRegex : inRegexs) {

			final Pattern p = Pattern.compile(aRegex);
			final Matcher m = p.matcher(inString);

			if (!wasNotFound && m.find(lastIndex)) {

				theResults.add(inString.substring(m.start(), m.end()));
				lastIndex = m.end();

			} else {

				wasNotFound = true;
				theResults.add(null);
			}
		}

		return theResults;
	}

	public static List<Integer> getOccurenceIndices(String inSubject, String inOccurence) {

		if (inSubject == null || inOccurence == null) {

			return new ArrayList<Integer>(0);
		}

		final List<Integer> theOccurences = new ArrayList<Integer>();
		int theLastIndex = 0;

		while (inSubject.length() >= theLastIndex + inOccurence.length()) {

			theLastIndex = inSubject.indexOf(inOccurence, theLastIndex);

			if (theLastIndex < 0) {

				break;
			}

			theOccurences.add(theLastIndex);
			theLastIndex += inOccurence.length();
		}

		return theOccurences;
	}

	/**
	 * Escape character is \
	 * 
	 * @param inString
	 * @param inSeparatorChar
	 * @param inTrimValues
	 * @return
	 */
	public static List<String> splitEscaped(String inString, String inSeparatorRegex, boolean inTrimValues) {

		return split(inString, "(?<!\\\\)" + inSeparatorRegex, inTrimValues);
	}

	public static List<String> split(String inString, String inRegex, boolean inTrimValues) {

		final List<String> r = new ArrayList<String>();

		if (inString == null) {
			return r;
		}

		for (final String string : inString.split(inRegex, -1)) {

			r.add(!inTrimValues ? string : string.trim());
		}

		return r;
	}

	/**
	 * Splits a {@link String} by the given separator.
	 * 
	 * @param inString
	 * @param inSeparator
	 * @return
	 */
	public static List<String> split(String inString, String inSeparator) {

		if (inString == null) {

			return new ArrayList<String>();

		} else if (inSeparator == null) {

			return new ArrayList<String>(Arrays.asList(inString));
		}

		final List<String> theResult = new ArrayList<String>();
		int idxStart = 0;
		int idxEnd = inString.indexOf(inSeparator, idxStart);
		while (idxStart <= idxEnd) {

			final String theValue = inString.substring(idxStart, idxEnd);
			theResult.add(theValue);

			idxStart = idxEnd + inSeparator.length();
			idxEnd = inString.indexOf(inSeparator, idxStart);
		}

		theResult.add(inString.substring(idxStart, inString.length()));

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

	public static String readStringFromClasspathResource(String inResource, Charset inCharset) throws IOException {

		return readString(getClasspathResource(inResource), inCharset);
	}

	public static InputStream getClasspathResource(String inResource) {

		return inResource == null ? null
				: Thread.currentThread().getContextClassLoader().getResourceAsStream(inResource);
	}

	public static byte[] toByteArray(File inFile) throws IOException {

		if (!isFile(inFile)) {
			return null;
		}

		final FileInputStream theFis = new FileInputStream(inFile);

		try {

			return toByteArray(theFis);

		} finally {

			close(theFis);
		}
	}

	public static byte[] toByteArray(InputStream inInputStream) throws IOException {

		if (inInputStream == null) {
			return EMPTY_BYTES;
		}

		final int blockSize = 1024;
		byte[] theBytes = new byte[blockSize];
		int reads = 0;
		int theNextReadSize = theBytes.length;

		while (inInputStream.available() > 0) {

			final int r = inInputStream.read(theBytes, reads, theNextReadSize);
			reads += r;

			if (r == blockSize) {

				theBytes = Arrays.copyOf(theBytes, reads + blockSize);
				theNextReadSize = blockSize;

			} else {

				theNextReadSize = blockSize - r;
			}
		}

		return reads == theBytes.length ? theBytes : Arrays.copyOf(theBytes, reads);
	}

	public static String escapeXml(String inValue) {

		return inValue == null ? null
				: inValue//
						.replace("&", "&amp;")//
						.replace("<", "&lt;")//
						.replace(">", "&gt;");
	}

	public static String escapeRegex(String inRegex) {

		// return inRegex == null ? null : Pattern.quote(inRegex);
		return inRegex == null ? null : Matcher.quoteReplacement(inRegex);
	}

	@SuppressWarnings("unchecked")
	public static <T> Set<T> createSet(T inValue) {

		return createSet(null, inValue);
	}

	public static <T> Set<T> createUnmodifiableSet(@SuppressWarnings("unchecked") T... inValues) {

		return Collections.unmodifiableSet(createSet(null, inValues));
	}

	public static <T> Set<T> createSet(Class<? extends Set<T>> inSetClass,
			@SuppressWarnings("unchecked") T... inValues) {

		@SuppressWarnings("unchecked")
		final Set<T> theSet = ReflectionUtils.newInstance(inSetClass == null ? HashSet.class : inSetClass, false);

		if (inValues != null) {

			for (final T anValue : inValues) {

				theSet.add(anValue);
			}
		}

		return theSet;
	}

	public static void streamAndClose(InputStream inIs, final IHandler<InputStream> inStreamHandler) throws Exception {

		try {

			if (inStreamHandler != null) {

				inStreamHandler.handle(inIs, 0);
			}

		} catch (final Exception e) {

			close(inIs);
		}
	}

	public static <T> void each(IHandler<T> inHandler, Collection<T> inValues) {

		if (inValues == null || inHandler == null) {
			return;
		}

		int theIdx = 0;

		for (final T aValue : inValues) {

			final int theIndex = theIdx++;

			try {

				if (!inHandler.handle(aValue, theIndex)) {

					break;
				}

			} catch (final Exception e) {

				try {

					if (!inHandler.handleException(theIndex, e, aValue)) {
						break;
					}

				} catch (final Exception e2) {

					throw toRuntimeException(e2);
				}
			}
		}

		try {

			inHandler.done(theIdx);

		} catch (final Exception e) {

			LOG.error(e);
		}
	}

	public static <T> void each(IHandler<T> inHandler, Iterable<T> inValues) throws Exception {

		each(inHandler, inValues == null ? null : inValues.iterator());
	}

	public static <T> void each(IHandler<T> inHandler, Iterator<T> inValues) throws Exception {

		if (inValues == null || inHandler == null) {
			return;
		}

		int theIdx = 0;

		while (inValues.hasNext()) {

			final int theIndex = theIdx++;
			final T aValue = inValues.next();

			try {

				if (!inHandler.handle(aValue, theIndex)) {

					break;
				}

			} catch (final Exception e) {

				try {

					if (!inHandler.handleException(theIndex, e, aValue)) {
						break;
					}

				} catch (final Exception e2) {

					throw toRuntimeException(e2);
				}
			}
		}

		try {

			inHandler.done(theIdx);

		} catch (final Exception e) {

			LOG.error(e);
		}
	}

	public static <T> void each(IHandler<T> inHandler, Enumeration<T> inValues) {

		if (inValues == null || inHandler == null) {
			return;
		}

		int theIdx = 0;

		while (inValues.hasMoreElements()) {

			final T aValue = inValues.nextElement();

			final int theIndex = theIdx++;

			try {

				if (!inHandler.handle(aValue, theIndex)) {

					break;
				}

			} catch (final Exception e) {

				try {

					if (!inHandler.handleException(theIndex, e, aValue)) {
						break;
					}

				} catch (final Exception e2) {

					throw toRuntimeException(e2);
				}
			}
		}

		try {

			inHandler.done(theIdx);

		} catch (final Exception e) {

			LOG.error(e);
		}
	}

	public static <T> void each(IHandler<T> inHandler, @SuppressWarnings("unchecked") T... inValues) {

		if (inValues == null || inHandler == null) {
			return;
		}

		int theIdx = 0;

		for (final T aValue : inValues) {

			final int theIndex = theIdx++;

			try {

				if (!inHandler.handle(aValue, theIndex)) {

					break;
				}

			} catch (final Exception e) {

				try {

					if (!inHandler.handleException(theIndex, e, aValue)) {
						break;
					}

				} catch (final Exception e2) {

					throw toRuntimeException(e2);
				}
			}
		}

		try {

			inHandler.done(theIdx);

		} catch (final Exception e) {

			LOG.error(e);
		}
	}

	public static <S, T> void each(IMapHandler<S, T> inHandler, Map<S, T> inMap) throws Exception {

		if (inHandler == null || inMap == null) {

			return;
		}

		final List<S> theKeys = new ArrayList<S>(inMap.keySet());
		final int theHandles = 0;

		while (!theKeys.isEmpty()) {

			final S theKey = theKeys.remove(0);
			final T theValue = inMap.get(theKey);

			try {

				if (!inHandler.handle(theKey, theValue)) {

					break;
				}

			} catch (final Exception e) {

				try {

					if (!inHandler.handleException(theKey, e, theValue)) {
						break;
					}

				} catch (final Exception e2) {

					throw toRuntimeException(e2);
				}
			}
		}

		try {

			inHandler.done(theHandles);

		} catch (final Exception e) {

			LOG.error(e);
		}
	}

	public static void handleLines(File inFile, String inCharset, ILineHandler inHandler) throws Exception {

		loadLines(inFile, inCharset, inHandler);
	}

	public static void handleLines(URL inUrl, String inCharset, ILineHandler inHandler) throws Exception {

		loadLines(inUrl, inCharset, inHandler);
	}

	public static void handleLines(InputStream inIs, String inCharset, IHandler<String> inHandler) throws Exception {

		final BufferedReader r = new BufferedReader(
				new InputStreamReader(inIs, inCharset == null ? Charset.defaultCharset() : Charset.forName(inCharset)));

		int theIdx = 0;

		while (r.ready()) {

			if (!inHandler.handle(r.readLine(), theIdx++)) {

				return;
			}
		}
	}

	/**
	 * @param inSrcFile
	 * @param inTargetFile
	 * @param inOverwrite
	 * @return the target files
	 * @throws IOException
	 */
	public static List<File> copyFiles(File inSrcFile, final File inTargetFile, final boolean inOverwrite)
			throws IOException {

		final List<File> theFiles = new ArrayList<File>();

		if (inSrcFile == null || !inSrcFile.exists()) {

			return theFiles;
		}

		final int theNamecutLength = inSrcFile.getCanonicalPath().length();

		final Traverser t = new Traverser(true) {

			@Override
			public boolean handleFile(File inSrcFile) throws Exception {

				final File theTargetFile = new File(inTargetFile,
						inSrcFile.getCanonicalPath().substring(theNamecutLength));

				copyFile(inSrcFile, theTargetFile, inOverwrite);

				theFiles.add(theTargetFile);

				return true;
			}
		};

		Utils.throwIOExceptionIfNotNull(t.getException());

		return theFiles;
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

				LOG.debug("copy: " + Utils.getCanonicalPathQuietly(inSrcFile, true) + " > "
						+ Utils.getCanonicalPathQuietly(inTargetFile, true) + "; " + getReadableDataSize(theBytes));
			}

			return theBytes;

		} finally {

			close(theFis, theFos);
		}
	}

	public static void handleMatches(final String inText, final String inOpen, final String inClose,
			IMatchHandler inMatchHandler) throws Exception {

		if (inText == null || inMatchHandler == null || inOpen == null || inClose == null) {

			return;
		}

		final int l = inText.length();
		int off = 0;
		int opens = 0;
		int openOff = 0;

		while (l - off > 0) {

			final int idxOpen = inText.indexOf(inOpen, off);
			final int idxClose = inText.indexOf(inClose, off);

			if (idxOpen > -1 && (idxOpen < idxClose || idxClose == -1)) {

				if (idxOpen > 0 && inText.charAt(idxOpen - 1) == '\\') {

					inMatchHandler.text(inText.substring(off, idxOpen + inOpen.length()), off);
					off = idxOpen + inOpen.length();
					continue;
				}

				// System.out.println("opened");
				opens++;

				if (opens == 1) {

					inMatchHandler.text(inText.substring(off, idxOpen), idxOpen);
					openOff = idxOpen;
				}

				off = idxOpen + inOpen.length();
				// System.out.println("rest open " + ": " +
				// inText.substring(o).replaceAll("\n", " - "));

			} else if (idxClose > -1 && (idxClose < idxOpen || idxOpen == -1)) {

				if (idxClose > 0 && inText.indexOf(idxClose - 1) == '\\'
						&& !(idxClose >= 2 && inText.indexOf(idxClose - 2) != '\\')) {

					off = idxClose + inClose.length();
					continue;
				}

				if (opens == 1) {

					inMatchHandler.match(inText.substring(openOff, idxClose + inClose.length()), idxClose);
					off = idxClose + inClose.length();

				} else if (opens == 0) {

					inMatchHandler.text(inText.substring(off, idxClose + inClose.length()), off);
				}

				opens -= (opens == 0 ? 0 : 1);

				off = idxClose + inClose.length();
				// System.out.println("rest close: " +
				// inText.substring(o).replaceAll("\n", " - "));

			} else {

				break;
			}
		}

		inMatchHandler.text(inText.substring(off), off);
	}

	public static void handleMatches(String inText, String inPattern, IMatchHandler inMatchHandler) throws Exception {

		if (inText == null || inMatchHandler == null || inPattern == null) {

			return;
		}

		final Matcher m = Pattern.compile(inPattern).matcher(inText);

		int off = 0;

		boolean run = true;

		while (run && m.find()) {

			run = inMatchHandler.text(inText.substring(off, m.start()), off);
			run = run == false ? false : inMatchHandler.match(inText.substring(m.start(), off = m.end()), m.start());
		}

		inMatchHandler.text(inText.substring(off), off);
	}

	public static interface IMatchHandler {

		boolean text(String inText, int inIndex) throws Exception;

		boolean match(String inText, int inIndex) throws Exception;
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

	public static String wrap(String string, Character c) {
		return c == null || string == null ? string : c + string + c;
	}

	public static String unwrap(final String string, Character inWrapChar) {

		return unwrap(string, inWrapChar, false);
	}

	public static String unwrap(final String string, Character inWrapChar, boolean inPostTrim) {

		String s = trimToEmpty(string);

		if (inWrapChar == null || s.length() < 2) {

			return string;

		} else if (s.charAt(0) != inWrapChar || s.charAt(s.length() - 1) != inWrapChar) {

			return string;

		} else if (s.length() == 2) {

			return "";
		}

		s = s.substring(1, s.length() - 1);
		s = !inPostTrim ? s : s.trim();

		return s;
	}

	public static String unwrap(String inValue, String inWrap, boolean inPreTrim, boolean inPostTrim) {

		if (inValue == null || inWrap == null) {
			return inValue;
		}

		inValue = !inPreTrim ? inValue : inValue.trim();

		String theResult = inValue;

		if (inValue.startsWith(inWrap) && inValue.endsWith(inWrap)) {

			theResult = inValue.substring(inWrap.length());
			theResult = !theResult.endsWith(inWrap) ? inValue
					: theResult.substring(0, theResult.length() - inWrap.length());
		}

		return !inPostTrim ? theResult : theResult.trim();
	}

	public static String getFilenameWithoutExtension(File aFile) {

		if (aFile == null) {
			return null;
		}

		final int idx = aFile.getName().lastIndexOf('.');

		return idx < 0 ? aFile.getName() : aFile.getName().substring(0, idx);
	}

	public static boolean isEqualStrings(String inS1, String inS2, boolean inCaseIgnore, boolean inTrim) {

		if (inS1 == inS2) {

			return true;

		} else if (inS1 == null || inS2 == null) {

			return false;
		}

		inS1 = !inCaseIgnore ? inS1 : inS1.toLowerCase();
		inS2 = !inCaseIgnore ? inS2 : inS2.toLowerCase();

		inS1 = !inTrim ? inS1 : inS1.trim();
		inS2 = !inTrim ? inS2 : inS2.trim();

		return inS1.equals(inS2);
	}

	public static <T extends Enum<T>> T lookupEnum(Class<T> inEnumClass, String inName) {

		if (inName == null) {
			return null;
		}

		inName = inName.trim();

		try {

			return Enum.valueOf(inEnumClass, inName);

		} catch (final Exception e) {

			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public static <T extends Enum<T>> T lookupEnum(Class<T> inEnumClass, String inName, boolean ignoreCase) {

		if (inEnumClass == null || inName == null) {
			return null;
		}

		inName = inName.trim();

		try {

			for (Enum<T> e : inEnumClass.getEnumConstants()) {
				if (ignoreCase && e.name().equalsIgnoreCase(inName)) {
					return (T) e;
				} else if (e.name().equals(inName)) {
					return (T) e;
				}
			}

		} catch (final Exception e) {
		}
		return null;
	}

	public static String replaceAll(String inTemplate, Map<String, String> inVars) {

		if (inTemplate == null || inVars == null) {
			return inTemplate;
		}

		for (final Entry<String, String> e : inVars.entrySet()) {

			if (e.getKey() == null) {
				continue;
			}

			inTemplate = inTemplate.replace(e.getKey(), e.getValue() == null ? e.getKey() : e.getValue());
		}

		return inTemplate;
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
	
	public static String cropNonLetters(String t) {
		
		return cropNonLetters( t, true, true );
	}
	
	

	private static String cropNonLetters(String inText, boolean inCropAtStart, boolean inCropAtEnd) {
		
		if ( inText == null || inText.isEmpty() ) { return inText; }
		
		int idxStart = 0, idxEnd = inText.length();
		
		if ( inCropAtStart ) {
			
			while( idxStart < idxEnd ) {
				
				final char c = inText.charAt(idxStart);
				
				if ( Character.isAlphabetic( c ) || Character.isDigit( c ) ) {
					break;
				}
				
				idxStart++;
			}
		}
		if ( inCropAtEnd ) {
			
			while( idxEnd > idxStart ) {
				
				final char c = inText.charAt(idxEnd-1);
				
				if ( Character.isAlphabetic( c ) || Character.isDigit( c ) ) {
					break;
				}
				
				idxEnd--;
			}
		}
		
		return inText.substring( idxStart, idxEnd );
	}

	public static InputStream getResource(String inDescriptor) {

		return inDescriptor == null ? null : getResource(inDescriptor, DEFAULT_RESOURCE_HIERARCHY);
	}

	public static InputStream getResource(String inDescriptor, EResourceType... inResourceTypes) {

		if (inDescriptor == null) {

			return null;
		}

		for (final EResourceType aResourceType : inResourceTypes == null ? DEFAULT_RESOURCE_HIERARCHY
				: inResourceTypes) {

			final InputStream theIs = aResourceType.getInputStream(inDescriptor);

			if (theIs != null) {

				return theIs;
			}
		}

		return null;
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

		System.out.println("\n");
		return new String(theChars);
	}

	public static String executeMain(Class<?> inClass, String... inArgs) throws Exception {

		final ByteArrayOutputStream theDev1 = new ByteArrayOutputStream();

		try {

			executeMain(inClass, inArgs, theDev1, null);

		} catch (final Exception e) {

			throw e;
		}

		return theDev1.toString();
	}

	public static void executeMain(Class<?> inClass, String[] inArgs, OutputStream inDev1, OutputStream inDev2)
			throws Exception {

		if (inClass == null) {

			throw new IllegalArgumentException("no class w/ main method given!");
		}

		final PrintStream theCurDev1 = System.out;
		final PrintStream theCurDev2 = System.err;

		final PrintStream theDev1 = inDev1 == null ? theCurDev1 : new PrintStream(inDev1);
		final PrintStream theDev2 = inDev2 == null ? theCurDev2 : new PrintStream(inDev2);

		try {

			final Method theMainMethod = ReflectionUtils.getMainMethod(inClass);

			if (theMainMethod == null) {

				throw new IllegalArgumentException("no main method given [class=" + inClass.getName() + "]");
			}

			System.setOut(theDev1);
			System.setErr(theDev2);

			ReflectionUtils.invokeMethod(theMainMethod, null, new Object[] { inArgs == null ? new String[0] : inArgs });

			theDev1.flush();
			theDev2.flush();

		} finally {

			System.setOut(theCurDev1);
			System.setErr(theCurDev2);
		}
	}

	public static int countNulls(Object... inValues) {

		if (inValues == null) {
			return 0;
		}

		int i = 0;

		for (final Object v : inValues) {

			i += v == null ? 1 : 0;
		}

		return i;
	}

	public static String toConcatenatedString(String inSeparator, boolean isNullEmptyString, List<?> inValues) {

		if (inValues == null) {
			return "";
		}

		inSeparator = inSeparator == null ? "" : inSeparator;
		final Object theDefault = isNullEmptyString ? "" : "null";

		final StringBuilder sb = new StringBuilder();

		for (int i = 0, l = inValues.size(); i < l; i++) {

			sb.append(Utils.defaultIfNull(inValues.get(i), theDefault).toString());

			if (i != l - 1) {

				sb.append(inSeparator);
			}

		}

		return sb.toString();
	}

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

		}

		return inDefault;
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

	public static boolean isOneOf(String inValue, boolean isIgnoreCase, String... inValues) {

		if (inValues == null) {

			return false;
		}

		for (final String aValue : inValues) {

			if (aValue == inValue) {
				return true;
			} else if (inValue != null && isIgnoreCase && inValue.equalsIgnoreCase(aValue)) {
				return true;
			} else if (inValue != null && !isIgnoreCase && inValue.equals(aValue)) {
				return true;
			}
		}

		return false;
	}

	public static String getFileType(File inFile) {

		if (inFile == null) {
			return null;
		}

		final int theIdx = inFile.getName().lastIndexOf('.');
		return theIdx < 0 ? null : inFile.getName().substring(theIdx + 1);
	}

	public static Date parseDate(String inFormat, String inDate) {

		try {

			return Timer.parse(inFormat, inDate);

		} catch (final Exception e) {
			// swallow
		}

		return null;
	}

	public static int getDecimalCount(Number inNumber) {

		final BigDecimal theBd = inNumber == null || inNumber instanceof BigDecimal ? (BigDecimal) inNumber
				: new BigDecimal(inNumber.toString());

		if (theBd == null) {
			return -1;
		} else if (theBd.remainder(BigDecimal.ONE).compareTo(BigDecimal.ZERO) == 0) {

			return 0;
		}

		final String theValue = theBd.toPlainString();

		return theValue.length() - theValue.lastIndexOf('.');
	}

	/**
	 * @param inArray
	 * @param inIndex
	 * @return the value at given index. Allows negative indices for indexing
	 *         from the end of the array.
	 */
	public static <T> T getValueAtIndex(T[] inArray, int inIndex) {

		if (inArray == null || inArray.length == 0) {

			return null;

		}

		inIndex = inIndex >= 0 ? inIndex : inArray.length + inIndex;

		return inIndex >= inArray.length ? null : inArray[inIndex];
	}

	/**
	 * @param inList
	 * @param inIndex
	 * @return the value at given index. Allows negative indices for indexing
	 *         from the end of the list.
	 */
	public static <T> T getValueAtIndex(List<T> inList, int inIndex) {

		if (inList == null || inList.isEmpty()) {

			return null;

		}

		inIndex = inIndex >= 0 ? inIndex : inList.size() + inIndex;

		return inIndex >= inList.size() || inIndex < 0 ? null : inList.get(inIndex);
	}

	/**
	 * @param inString
	 *            text to be manipulated
	 * @param inRemoval
	 *            string to be removed
	 * @param inStart
	 *            remove at the start
	 * @param inEnd
	 *            remove at the end
	 * @param inTrim
	 *            trim after! removing results
	 * @return
	 */
	public static String removeFirstLastChars(String inString, String inRemoval, boolean inStart, boolean inEnd,
			boolean inTrim) {

		if (inString == null || inRemoval == null) {

			return inString;
		}

		final int theRemovalLen = inRemoval.length();

		while (inStart && inString.startsWith(inRemoval)) {

			inString = inString.substring(theRemovalLen);
			inString = !inTrim ? inString : inString.trim();
		}

		while (inEnd && inString.endsWith(inRemoval)) {

			inString = inString.substring(0, inString.lastIndexOf(inRemoval));
			inString = !inTrim ? inString : inString.trim();
		}

		return inString;
	}

	@SuppressWarnings("unchecked")
	public static <T> T[] toArray(boolean inIncludeNulls, T... inValues) {

		final List<Object> theValues = new ArrayList<Object>();

		if (inValues != null) {

			for (final T v : inValues) {

				if (inIncludeNulls || v != null) {

					theValues.add(v);
				}
			}
		}

		return (T[]) theValues.toArray();
	}

	/**
	 * Adds values to the given value until it is a value within the range of
	 * min and max by adding/substracting the steps value
	 * 
	 * @param inValue
	 * @param inMin
	 * @param inMax
	 * @return
	 */
	public static Integer convertIntoRange(Integer inValue, int inSteps, int inMin, int inMax) {

		if (inValue == null) {
			return null;
		}

		if (inMin > inMax) {

			final int t = inMax;
			inMax = inMin;
			inMin = t;
		}

		if (inValue >= inMin && inValue <= inMax) {
			return inValue;
		}

		while (inValue < inMin) {

			inValue += inSteps;
		}

		while (inValue > inMax) {

			inValue -= inSteps;
		}

		return inValue < inMin || inValue > inMax ? null : inValue;
	}

	public static boolean isTrue(Boolean inBoolean) {

		return inBoolean != null && inBoolean;
	}

	public static boolean isFalse(Boolean inBoolean) {

		return inBoolean != null && !inBoolean;
	}

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

	public static <T, S> void handleEntrys(Map<T, S> inMap, IHandler<Entry<T, S>> inHandler) throws Exception {

		if (inMap == null || inHandler == null) {
			return;
		}

		int theIndexs = 0;

		for (final Entry<T, S> aEntry : inMap.entrySet()) {

			if (inHandler.handle(aEntry, theIndexs++) == false) {
				return;
			}

		}
	}

	public static Throwable findThrowable(Throwable inThrowable, Class<? extends Throwable> inThrowableClass) {

		if (inThrowable == null || inThrowableClass == null) {
			return null;
		}

		Throwable t = inThrowable;

		while (t != null) {

			if (inThrowableClass.isAssignableFrom(t.getClass())) {

				return t;
			}

			t = t.getCause();
		}

		return null;
	}

	public static <V> Collection<V> getMapValues(Map<?, V> inMap) {

		return getMapValues(inMap, new HashSet<V>());
	}

	public static <V> Collection<V> getMapValues(Map<?, V> inMap, Collection<V> inDefault) {

		return inMap == null ? inDefault : defaultIfNull(inMap.values(), inDefault);
	}

	public static <K> Collection<K> getMapKeys(Map<K, ?> inMap) {

		return getMapKeys(inMap, new HashSet<K>());
	}

	public static <K> Collection<K> getMapKeys(Map<K, ?> inMap, Collection<K> inDefault) {

		return inMap == null ? inDefault : defaultIfNull(inMap.keySet(), inDefault);
	}

	public static <S, T> T getMapValue(Map<S, T> inMap, S inKey) {

		return getMapValue(inMap, inKey, null);
	}

	public static <S, T> T getMapValue(Map<S, T> inMap, S inKey, T inDefault) {

		return inMap == null ? inDefault : defaultIfNull(inMap.get(inKey), inDefault);
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

	public static boolean isElementInArray(Object[] inValues, Object inValue) {

		if (inValues == null) {
			return false;
		}

		for (final Object aValue : inValues) {

			if (aValue == inValue) {
				return true;
				// } else if (aValue != null && aValue.equals(inValue)) {
				// return true;
			}
		}

		return false;
	}

	public static InputStream getFileInputStreamSafe(File inFile) {
		try {
			return inFile == null || !isFile(inFile) ? null : new FileInputStream(inFile);
		} catch (final FileNotFoundException e) {
			return null;
		}
	}

	/**
	 * @param inValue
	 * @return casted value or given default if {@link ClassCastException} would
	 *         occur
	 */
	@SuppressWarnings("unchecked")
	public static <T> T castSafe(Class<T> inClass, Object inValue, T inDefault) {

		if (inClass == null) {
			throw new IllegalArgumentException("no target class specified");
		} else if (inValue != null && !inClass.isAssignableFrom(inValue.getClass())) {
			return inDefault;
		}

		try {

			return (T) inValue;

		} catch (Exception e) {

			return inDefault;
		}
	}

	/**
	 * @param inValue
	 * @return casted value or <code>null</code> if {@link ClassCastException}
	 *         would occur
	 */
	public static <T> T castSafe(Class<? extends T> inClass, Object inValue) {

		return castSafe(inClass, inValue, null);
	}

	public static Integer castInteger(Object inObject) {

		return castInteger(inObject, null);
	}

	public static Integer castInteger(Object inObject, Integer inDefault) {

		try {

			return (Integer) inObject;

		} catch (final Exception e) {
			return inDefault;
		}
	}

	public static Boolean castBoolean(Object inObject) {

		return castBoolean(inObject, null);
	}

	public static Boolean castBoolean(Object inObject, Boolean inDefault) {

		try {

			return (Boolean) inObject;

		} catch (final Exception e) {
			return inDefault;
		}
	}

	public static boolean isInteger(BigDecimal inValue) {

		try {

			inValue.intValueExact();

		} catch (final Exception e) {
			// swallow
		}

		return false;
	}

	public static boolean isLong(BigDecimal inValue) {

		try {

			inValue.longValueExact();

		} catch (final Exception e) {
			// swallow
		}

		return false;
	}

	public static boolean isDouble(BigDecimal inValue) {

		try {

			return isLessEqual(BD_DOUBLE_MAX, inValue) && isLarger(BD_DOUBLE_MIN, inValue);

		} catch (final Exception e) {
			// swallow
		}

		return false;
	}

	public static boolean isFloat(BigDecimal inValue) {

		try {

			return isLessEqual(BD_FLOAT_MAX, inValue) && isLarger(BD_FLOAT_MIN, inValue);

		} catch (final Exception e) {
			// swallow
		}

		return false;
	}

	public static boolean equals(Object inO1, Object inO2) {

		return equals(inO1, inO2, true);
	}

	public static boolean notEquals(Object inO1, Object inO2) {

		return !equals(inO1, inO2, true);
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

	public static boolean notEquals(Object inO1, Object inO2, boolean isBothNullEqual) {

		return !equals(inO1, inO2, isBothNullEqual);
	}

	public static long getMillis(int inDays, int inHours, int inMinutes, int inSeconds) {

		return getMillis(inDays, inHours, inMinutes, inSeconds, 0);
	}

	public static long getMillis(int inDays, int inHours, int inMinutes, int inSeconds, int inMillis) {

		return (inMillis) + (inSeconds * MILLIS_1_SECOND) + (inMinutes * MILLIS_1_MINUTE) + (inHours * MILLIS_1_HOUR)
				+ (inDays * MILLIS_1_DAY);
	}

	public static String firstLetterLowerCase(String inString) {

		return inString == null || inString.isEmpty() ? inString
				: Character.toLowerCase(inString.charAt(0)) + inString.substring(1);
	}

	@SafeVarargs
	public static <T> T getFirstNotNull(T... inValues) {

		if (inValues != null) {

			for (T aValue : inValues) {

				if (aValue != null) {
					return aValue;
				}
			}
		}

		return null;
	}

	public static String formatSafe(String inTemplate, Object... inValues) {

		try {

			return String.format(inTemplate, inValues);

		} catch (Exception e) {
			// swallow
		}

		final StringBuilder theSb = new StringBuilder();

		if (inTemplate != null) {

			theSb.append("<bad string format template:").append(inTemplate).append("> ");
		}

		if (inValues != null) {

			for (int i = 0, l = inValues.length; i < l; i++) {

				theSb.append(inValues[i]);
				if (i != l - 1) {
					theSb.append(", ");
				}
			}
		}

		return theSb.toString();
	}

	public static String getCanonicalSubPath(File inBasepathDir, File inFile) {
		return getCanonicalSubPath(inBasepathDir, inFile, false);
	}

	/**
	 * 
	 * @param theBasepath
	 * @param inFile
	 * @return the path part which does not start with the basepath
	 */
	public static String getCanonicalSubPath(File inBasepathDir, File inFile, boolean inQuiet) {

		inBasepathDir = getCanonicalPathQuietly(inBasepathDir);
		inFile = getCanonicalPathQuietly(inFile);
		if (!isDirectory(inBasepathDir) && inQuiet) {
			return null;
		} else if (!isDirectory(inBasepathDir)) {
			throw new IllegalArgumentException("basepath is no directory");
		} else if (inFile == null) {
			return null;
		} else if (!inFile.getAbsolutePath().startsWith(inBasepathDir.getAbsolutePath()) && inQuiet) {
			return null;
		} else if (!inFile.getAbsolutePath().startsWith(inBasepathDir.getAbsolutePath())) {
			throw new IllegalArgumentException("file is no sub file of basepath");
		}

		return inFile.getAbsolutePath().substring(inBasepathDir.getAbsolutePath().length());
	}

	@SafeVarargs
	public static <T> List<T> mergeCollections(Collection<T>... inCollections) {

		return addCollections(false, inCollections);
	}

	@SafeVarargs
	public static <T> List<T> addCollections(Collection<T>... inCollections) {

		return addCollections(true, inCollections);
	}

	@SafeVarargs
	public static <T> List<T> addCollections(boolean inAddDuplicates, Collection<T>... inCollections) {

		final List<T> theList = new ArrayList<T>();

		if (inCollections != null) {

			for (Collection<T> aCollection : inCollections) {

				if (aCollection != null) {

					for (T anItem : aCollection) {

						if (!inAddDuplicates && theList.contains(anItem)) {
							continue;
						}
						theList.add(anItem);
					}
				}
			}
		}

		return theList;
	}

	public static String readFile(Reader inReader) throws IOException {

		if (inReader == null) {
			return null;
		}

		final StringBuilder theSb = new StringBuilder();

		final char[] cbuf = new char[1024];

		while (inReader.ready()) {

			final int r = inReader.read(cbuf);
			theSb.append(cbuf, 0, r);
		}

		return theSb.toString();
	}

	public static Long getLongWithinBounds(Long inValue, Long inMinAllowed, Long inMaxAllowed) {

		if (inValue == null) {
			return null;
		}

		if (inMinAllowed != null && inMaxAllowed != null && inMinAllowed > inMaxAllowed) {

			final Long t = inMinAllowed;
			inMinAllowed = inMaxAllowed;
			inMaxAllowed = t;
		}

		if (inMinAllowed != null && inValue < inMinAllowed) {
			inValue = inMinAllowed;
		}
		if (inMaxAllowed != null && inValue > inMaxAllowed) {
			inValue = inMaxAllowed;
		}

		return inValue;
	}

	public static Integer getIntegerWithinBounds(Integer inValue, Integer inMinAllowed, Integer inMaxAllowed) {

		if (inValue == null) {
			return null;
		}

		if (inMinAllowed != null && inMaxAllowed != null && inMinAllowed > inMaxAllowed) {

			final Integer t = inMinAllowed;
			inMinAllowed = inMaxAllowed;
			inMaxAllowed = t;
		}

		if (inMinAllowed != null && inValue < inMinAllowed) {
			inValue = inMinAllowed;
		}
		if (inMaxAllowed != null && inValue > inMaxAllowed) {
			inValue = inMaxAllowed;
		}

		return inValue;
	}
	
	public enum EResourceType {

		URL {

			@Override
			public URL getUrl(String inDescriptor) {

				try {

					return inDescriptor == null ? null : new URL(inDescriptor);

				} catch (final Exception e) {
					return null;
				}
			}
		},
		FILE {

			@Override
			public URL getUrl(String inDescriptor) {

				try {
					return inDescriptor == null ? null : new File(inDescriptor).toURI().toURL();
				} catch (final MalformedURLException e) {

					return null;
				}
			}

		},

		CLASSPATH {

			@Override
			public URL getUrl(String inDescriptor) {

				return inDescriptor == null ? null : ClassLoader.getSystemClassLoader().getResource(inDescriptor);
			}
		},
		CONTEXTLOADER_CLASSPATH {

			@Override
			public URL getUrl(String inDescriptor) {

				return inDescriptor == null ? null
						: Thread.currentThread().getContextClassLoader().getResource(inDescriptor);
			}
		};

		public URL getUrl(String inDescriptor) {

			throw new UnsupportedOperationException();
		}

		public InputStream getInputStream(String inDescriptor) {

			try {

				return getUrl(inDescriptor).openStream();

			} catch (final Exception e) {

				return null;
			}
		}
	}
	
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

	@SuppressWarnings("unchecked")
	public static <T> Iterable<T> toIterable(final Iterator<T> iterator) {
		
		if ( iterator == null ) { return ITERABLE_NONE; }
		
		return new Iterable<T>() { 
			
			boolean isInvoked = false;

			@Override
			public Iterator<T> iterator() {
				
				if ( isInvoked ) {
					
					throw new IllegalStateException("iterator() was already invoked [iterator="+ iterator +"]");
				}
				
				isInvoked = true;
				
				return iterator;
			}
		};
	}

	public static <T> List<T> toList(Enumeration<T> inEnum) {
	
		final List<T> l = new ArrayList<>();
		
		if ( inEnum == null ) { return l; }
		
		while( inEnum.hasMoreElements() ) {
			l.add( inEnum.nextElement() );
		}
		
		return l;
	}
	
	public static <T> List<T> toList(@SuppressWarnings("unchecked") T... inValues) {
		
		if ( inValues == null ) {
			
			return new ArrayList<>(0);
		}
		
		final List<T> theList = new ArrayList<>( inValues.length );
		
		for (T aValue : inValues) {
			
			theList.add( aValue );
			
		}
		
		return theList;
	}
	
	public static class Dev0OutputStream extends OutputStream {
		public final static Dev0OutputStream INSTANCE = new Dev0OutputStream();
		private Dev0OutputStream() {}
		public static Dev0OutputStream getInstance() {
			return INSTANCE;
		}
		@Override
		public void write(int b) throws IOException {}
	}
	
	/**
	 * from https://www.mkyong.com/java/how-to-decompress-files-from-a-zip-file/
	 * @param inZipFile
	 * @param inTargetDir
	 * @throws IOException 
	 */
	public static void unzip(File inZipFile, File inTargetDir) throws IOException {
		
		if ( inTargetDir == null ) {
			inTargetDir = getCanonicalPathQuietly(new File(".", inZipFile.getName()+".unzipped") );
		}
		inTargetDir.mkdirs();
		
		 byte[] buffer = new byte[1024];

		 Set<Closeable> closeCandiodates = new HashSet<>();
		 
	     try{

	    	//get the zip file content
	    	ZipInputStream zis =
	    		new ZipInputStream(new FileInputStream( inZipFile ));
	    	
	    	closeCandiodates.add( zis );
	    	
	    	//get the zipped file list entry
	    	ZipEntry ze = zis.getNextEntry();

	    	while(ze!=null){

	    	   String fileName = ze.getName();
	           File newFile = new File( inTargetDir  + File.separator + fileName);

	            //create all non exists folders
	            //else you will hit FileNotFoundException for compressed folder
	            new File(newFile.getParent()).mkdirs();

	            final FileOutputStream fos = new FileOutputStream(newFile);
	            closeCandiodates.add( fos );
	            
	            int len;
	            
	            while ((len = zis.read(buffer)) > 0) {
	            	fos.write(buffer, 0, len);
	            }

	            Utils.close( fos );
	            ze = zis.getNextEntry();
	    	}

	        zis.closeEntry();
	    	zis.close();

	    } finally  {
	       
	    	close( closeCandiodates );
	    }
	}
	
}