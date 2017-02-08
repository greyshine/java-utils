package de.greyshine.utils.deprecated;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

public abstract class Timer {
	
	public static final String DATE_YYYYMMDD = "yyyyMMdd";
	public static final String DATE_YYYYMMDDHHMMSS = "yyyyMMddHHmmss";
	public static final String DATE_YYYYMMDDHHMMSSsss = "yyyyMMddHHmmssSSS";
	public static final String DATE_DD_MM_YYYY = "dd.MM.yyyy";
	public static final String DATE_HH_MM = "HH:mm";
	public static final String DATE_HH_MM_SS = "HH:mm:ss";
	public static final String DATE_DD_MM_YYYY_HH_MM_SS = "dd.MM.yyyy HH:mm:ss";
	public static final String DATE_YYYY_MM_DD = "yyyy-MM-dd";
	public static final String DATE_YYYY_MM_DD_HH_MM = "yyyy-MM-dd HH:mm";
	public static final String DATE_YYYY_MM_DD_HH_MM_SS = "yyyy-MM-dd HH:mm:ss";
	public static final String DATE_YYYY_MM_DD_HH_MM_SS_SSS = "yyyy-MM-dd HH:mm:ss.SSS";
	public static final String DATE_ISO8601_ZULU = "yyyy-MM-dd'T'HH:mm:ss'Z'";
	public static final String DATE_ISO8601_ZULU_MS = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";

	public final static long SECOND_IN_MILLIS = 1000;
	public final static long MINUTE_IN_MILLIS = 60 * SECOND_IN_MILLIS;
	public final static long HOUR_IN_MILLIS = 60 * MINUTE_IN_MILLIS;
	public final static long DAY_IN_MILLIS = 24 * HOUR_IN_MILLIS;

	public enum DateFormat {

		YYYYMMDD(DATE_YYYYMMDD),		//
		YYYYMMDDHHMMSS(DATE_YYYYMMDDHHMMSS),		//
		YYYYMMDDHHMMSSsss(DATE_YYYYMMDDHHMMSSsss),		//
		DD_MM_YYYY(DATE_DD_MM_YYYY),		//
		DD_MM_YYYY_HH_MM_SS(DATE_DD_MM_YYYY_HH_MM_SS),		//
		YYYY_MM_DD_HH_MM_SS_SSS(DATE_YYYY_MM_DD_HH_MM_SS_SSS),		//
		ISO8601_ZULU(DATE_ISO8601_ZULU),		//
		;

		public final String format;

		private DateFormat(String inFormat) {

			format = inFormat;
		}

		public Date parse(String inDate) {

			try {
				return Timer.parse(format, inDate);
			} catch (final ParseException e) {
				return null;
			}
		}
	}

	public final static TimeZone TIMEZONE_UTC = TimeZone.getTimeZone("UTC");

	private final static ThreadLocal<Map<String, SimpleDateFormat>> TL_SDFS = new ThreadLocal<Map<String, SimpleDateFormat>>() {

		@Override
		protected Map<String, SimpleDateFormat> initialValue() {

			final Map<String, SimpleDateFormat> theSdfs = new HashMap<String, SimpleDateFormat>();

			for (final DateFormat aDf : DateFormat.values()) {

				theSdfs.put(aDf.format, new SimpleDateFormat(aDf.format));
			}

			for (final String aSdf : new String[] { DATE_YYYYMMDDHHMMSS, DATE_YYYYMMDDHHMMSSsss, DATE_DD_MM_YYYY, DATE_HH_MM, DATE_HH_MM_SS, DATE_ISO8601_ZULU }) {

				theSdfs.put(aSdf, new SimpleDateFormat(aSdf));
			}

			return theSdfs;
		}

	};

	public final static Timer SYSTEM = new Timer() {

		@Override
		public long millis() {

			return System.currentTimeMillis();
		}
	};

	private static Timer INSTANCE = Timer.SYSTEM;

	static {

		final String theDate = System.getProperty("sidi.date-now");

		if (theDate != null && !theDate.trim().isEmpty() && theDate.matches("[0-9]{6," + "yyyyMMddHHmmss".length() + "}")) {

			try {

				INSTANCE = createRelativeTimer(new SimpleDateFormat("yyyyMMddHHmmss".substring(0, theDate.length())).parse(theDate).getTime());

			} catch (final ParseException e) {

				System.err.println("Unable to set current date: " + theDate);
			}
		}
	}

	public static Timer getInstance() {

		return INSTANCE;
	}

	/**
	 * @return current {@link Timer}'s {@link TimeZone} or the defualt if none
	 *         set
	 */
	public static TimeZone getTimeZone() {

		return Utils.defaultIfNull(INSTANCE.timezone(), TimeZone.getDefault());
	}

	public static void setInstance(Timer inTimer) {

		INSTANCE = Utils.defaultIfNull(inTimer, SYSTEM);
	}

	public static Timer createRelativeTimer(final long inMillis) {

		return createRelativeTimer(null, inMillis);
	}

	public static Timer createRelativeTimer(final TimeZone inTimeZone, final long inMillis) {

		final TimeZone tz = Utils.defaultIfNull(inTimeZone, TimeZone.getDefault());

		return new Timer() {

			final long basetime = System.currentTimeMillis();

			@Override
			public long millis() {

				return inMillis + System.currentTimeMillis() - basetime;
			}

			@Override
			public TimeZone timezone() {

				return tz;
			}
		};
	}

	public static Timer createRelativeTimer(int year, int month, int day, int hours, int minutes, int seconds) {

		return createRelativeTimer(null, year, month, day, hours, minutes, seconds);
	}

	public static Timer createRelativeTimer(TimeZone inTimeZone, int year, int month, int day, int hours, int minutes, int seconds) {

		final Calendar c = Calendar.getInstance();
		c.setTimeZone(inTimeZone != null ? inTimeZone : TimeZone.getDefault());
		c.set(Calendar.MILLISECOND, 0);
		c.set(year, month - 1, day, hours, minutes, seconds);

		return createRelativeTimer(c.getTimeInMillis());
	}

	public static Timer createAbslouteTimer(int year, int month, int day, int hours, int minutes, int seconds) {

		final Calendar c = Calendar.getInstance();
		c.set(Calendar.MILLISECOND, 0);
		c.set(year, month - 1, day, hours, minutes, seconds);

		return new Timer() {

			final long time = c.getTimeInMillis();

			@Override
			public long millis() {

				return time;
			}
		};
	}

	public abstract long millis();

	public TimeZone timezone() {
		return TimeZone.getDefault();
	}

	public Locale locale() {
		return Locale.getDefault();
	}

	public final Date date() {
		return calendar().getTime();
	}

	public final Calendar calendar() {
	
		final TimeZone t = timezone();
		final Locale l = locale();
		final Calendar c = Calendar.getInstance(t != null ? t : TimeZone.getDefault(), l != null ? l : Locale.getDefault());
		c.setTimeInMillis(millis());
		return c;
	}


	public final GregorianCalendar gregorianCalendar() {

		final GregorianCalendar c = new GregorianCalendar();
		c.setTimeInMillis(millis());
		return c;
	}

	public static String getDate(String inFormat) {

		return inFormat == null ? Timer.getDate().toString() : Timer.format(inFormat, Timer.getDate());
	}

	public static Date getDate() {
		return INSTANCE.date();
	}

	public static long getMillis() {
		return INSTANCE.millis();
	}

	public static GregorianCalendar getGregorianCalendar() {
		return INSTANCE.gregorianCalendar();
	}

	public static SimpleDateFormat getSimpleDateFormat(String inFormat) {

		SimpleDateFormat theSdf = TL_SDFS.get().get(inFormat);

		if (theSdf == null) {
			TL_SDFS.get().put(inFormat, theSdf = new SimpleDateFormat(inFormat));
		}

		return theSdf;
	}

	public static Date convertToUtc(Date inDate) {
		
		if ( inDate == null ) { return null; }
		
		final Calendar c = toCalendar(inDate);

		if (TIMEZONE_UTC.equals(c.getTimeZone())) {
			return inDate;
		}

		c.setTimeZone(TIMEZONE_UTC);
		c.add(Calendar.MILLISECOND, -1 * getTimeZone().getOffset(inDate.getTime()));
		return c.getTime();
	}

	/**
	 * converts not regarding the timezone of the {@link Date}
	 * 
	 * @param inDate
	 * @return
	 */
	public static Date convertFromUtc(Date inDate) {

		if (inDate == null) {
			return null;
		}

		final Calendar c = toCalendar(inDate);
		c.add(Calendar.MILLISECOND, getTimeZone().getOffset(inDate.getTime()));
		c.setTimeZone(getTimeZone());
		return c.getTime();
	}

	public static String format(String inFormat) {
		return format(inFormat, Timer.getDate());
	}

	public static String format(String inFormat, Object inDate) {

		if (inDate == null) {

			return null;

		} else if (inDate instanceof Date) {

			return format(inFormat, (Date) inDate);

		} else if (inDate instanceof Calendar) {

			return format(inFormat, (Calendar) inDate);

		} else if (inDate instanceof Long) {

			return format(inFormat, (Long) inDate);
		}

		return inDate.toString();
	}

	public static String format(String inFormat, Long inDate) {

		return inDate == null ? null : format(inFormat, (long) inDate);
	}

	public static String format(String inFormat, long inDate) {

		return format(inFormat, new Date(inDate));
	}

	public static String format(String inFormat, Date inDate) {

		return inDate == null ? null : getSimpleDateFormat(inFormat).format(inDate);
	}

	public static String formatISO8610Zulu() {
		return formatISO8610Zulu(getDate());
	}

	public static String formatISO8610Zulu(long inMillis) {

		return formatISO8610Zulu(toDate(inMillis));
	}

	public static String formatISO8610Zulu(Date inDate) {

		return inDate == null ? null : format(DATE_ISO8601_ZULU, convertToUtc(inDate));
	}
	
	public static String formatISO8610Zulu(Calendar inDate) {

		return inDate == null ? null : format(DATE_ISO8601_ZULU, inDate.getTime());
	}
	
	public static String format(String inFormat, Calendar inDate) {

		return getSimpleDateFormat(inFormat).format(inDate);
	}

	public static Date parse(String inFormat, String inDate) throws ParseException {

		return inDate == null ? null : getSimpleDateFormat(inFormat).parse(inDate);
	}
	
	public static Date parse(String inFormat, String inDate, Date inDefault) {
		
		try {

			return parse(inFormat, inDate);
			
		} catch (Exception e) {
			return inDefault;
		}
	}

	public static Date parse(String[] inFormats, String inDate) {

		Date theDate = null;

		if (inFormats != null && inDate != null) {
			for (final String aFormat : inFormats) {

				try {
					if ((theDate = parse(aFormat, inDate)) != null) {
						return theDate;
					}
				} catch (final ParseException e) {
					// swallow
				}
			}
		}

		return theDate;
	}

	public static long getDaysBetween(Date inStartDate, Date inEndDate) {

		Calendar theStart = Calendar.getInstance();
		theStart.setTime(inStartDate);
		Calendar theEnd = Calendar.getInstance();
		theEnd.setTime(inEndDate);

		if (theStart.after(theEnd)) {
			final Calendar temp = theEnd;
			theEnd = theStart;
			theStart = temp;
		}

		long theDifference = 0;
		while (theStart.before(theEnd)) {
			theStart.add(Calendar.DATE, 1);
			theDifference++;
		}

		return theDifference;
	}

	public static Date createDate(int inYear, int inMonth, int inDayOfMonth) {
		return createCalendar(inYear, inMonth, inDayOfMonth).getTime();
	}

	public static Calendar createCalendar(int inYear, int inMonth, int inDayOfMonth) {
		return createCalendar(inYear, inMonth, inDayOfMonth, 0, 0, 0, 0);
	}

	public static Date createDate(int inYear, int inMonth, int inDayOfMonth, int in24Hour, int inMinute, int inSecond) {
		return createCalendar(inYear, inMonth, inDayOfMonth, in24Hour, inMinute, inSecond).getTime();
	}

	public static Calendar createCalendar(int inYear, int inMonth, int inDayOfMonth, int in24Hour, int inMinute, int inSecond) {
		return createCalendar(inYear, inMonth, inDayOfMonth, in24Hour, inMinute, inSecond, 0);
	}

	public static Date createDate(int inYear, int inMonth, int inDayOfMonth, int in24Hour, int inMinute, int inSecond, int inMillicseconds) {
		return createCalendar(inYear, inMonth, inDayOfMonth, in24Hour, inMinute, inSecond, inMillicseconds).getTime();
	}

	public static Calendar createCalendar(int inYear, int inMonth, int inDayOfMonth, int in24Hour, int inMinute, int inSecond, int inMillicseconds) {
		final Calendar theCalendar = Calendar.getInstance();
		theCalendar.set(Calendar.YEAR, inYear);
		theCalendar.set(Calendar.MONTH, inMonth - 1);
		theCalendar.set(Calendar.DATE, inDayOfMonth);
		theCalendar.set(Calendar.HOUR_OF_DAY, in24Hour);
		theCalendar.set(Calendar.MINUTE, inMinute);
		theCalendar.set(Calendar.SECOND, inSecond);
		theCalendar.set(Calendar.MILLISECOND, inMillicseconds);
		return theCalendar;
	}

	public static Date add(Date inBaseDate, int inCalendarField, int inValue) {
		final Calendar theTime = Calendar.getInstance();
		theTime.setTime(inBaseDate == null ? Timer.getDate() : inBaseDate);
		theTime.add(inCalendarField, inValue);
		return theTime.getTime();
	}

	public static Calendar setZeroOclock(Calendar inCalendar) {
		if ( inCalendar == null ) { return null; }
		inCalendar.set(Calendar.HOUR_OF_DAY, 0);
		inCalendar.set(Calendar.MINUTE, 0);
		inCalendar.set(Calendar.SECOND, 0);
		inCalendar.set(Calendar.MILLISECOND, 0);
		return inCalendar;
	}
	
	public static Calendar set2359599999Oclock(Calendar inCalendar) {
		if ( inCalendar == null ) { return null; }
		inCalendar.set(Calendar.HOUR_OF_DAY, 23);
		inCalendar.set(Calendar.MINUTE, 59);
		inCalendar.set(Calendar.SECOND, 59);
		inCalendar.set(Calendar.MILLISECOND, 999);
		return inCalendar;
	}

	public static int getYearsBetween(Date inStartDate, Date inEndDate) {
		Calendar theStart = Calendar.getInstance();
		theStart.setTime(inStartDate);
		Calendar theEnd = Calendar.getInstance();
		theEnd.setTime(inEndDate);

		if (theStart.after(theEnd)) {
			final Calendar temp = theEnd;
			theEnd = theStart;
			theStart = temp;
		}

		return (int) ((theEnd.getTime().getTime() - theStart.getTime().getTime()) / 1000 / 60 / 60 / 24 / 365.25);
	}

	public static int toYYYYMMDD(Date inDate) {

		return Integer.parseInt(format(DATE_YYYYMMDD, inDate != null ? inDate : Timer.getDate()));
	}

	public static long toYYYYMMDDHHmmSSsss() {
		return toYYYYMMDDHHmmSSsss( getDate() );
	}
	
	public static long toYYYYMMDDHHmmSSsss(Date inDate) {
		
		return Long.parseLong(format(DATE_YYYYMMDDHHMMSSsss, inDate != null ? inDate : Timer.getDate()));
	}

	public static Date fromYYYYMMDD(int inDate) {

		try {

			return parse(DATE_YYYYMMDD, Integer.toString(inDate));

		} catch (final ParseException e) {
			throw Utils.toRuntimeException(e);
		}
	}

	public static String getReadableDaysHoursMinutesSeconds(long inTimeInMillis) {

		final StringBuilder sb = new StringBuilder(inTimeInMillis < 0 ? "-" : "");
		inTimeInMillis *= inTimeInMillis < 0 ? -1 : 1;

		long x = inTimeInMillis / (DAY_IN_MILLIS);
		sb.append(x).append("d ");
		inTimeInMillis -= (x * DAY_IN_MILLIS);

		x = inTimeInMillis / (HOUR_IN_MILLIS);
		sb.append(x).append("h ");
		inTimeInMillis -= (x * HOUR_IN_MILLIS);

		x = inTimeInMillis / MINUTE_IN_MILLIS;
		sb.append(x).append("m ");
		inTimeInMillis -= (x * MINUTE_IN_MILLIS);

		x = inTimeInMillis / SECOND_IN_MILLIS;
		sb.append(x).append("s ");
		inTimeInMillis -= (x * SECOND_IN_MILLIS);

		sb.append(inTimeInMillis).append("ms");

		return sb.toString();
	}

	public static Date setPartial(Date inDate, int inField, int inValue) {
		
		final Calendar c = toCalendar(inDate);

		if (c != null) {
			
			c.set(inField, inValue);
		}
		
		return c == null ? null : c.getTime();
	}

	public static Calendar toCalendar(Date inDate) {

		Calendar c = null;

		if (inDate != null) {

			c = Calendar.getInstance();
			c.setTime(inDate);
		}

		return c;
	}

	public static Date toZeroTime(Date inDate) {

		final Calendar c = toCalendar(inDate);

		if (c != null) {

			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);
			c.set(Calendar.MILLISECOND, 0);
		}

		return c == null ? null : c.getTime();
	}

	public static Date toDate(Object inValue) {
		
		return toDate(inValue, null);
	}
	
	public static Date toDate(Object inValue, Date inDefault) {

		if (inValue == null || inValue instanceof Date) {

			return (Date) inValue;

		} else if (inValue instanceof Calendar) {

			return ((Calendar) inValue).getTime();

		} else if (inValue instanceof Long) {

			return new Date((Long) inValue);
		
		} else if ( inValue instanceof String ) {
			
			for (final DateFormat aDf : DateFormat.values()) {

				final Date d = aDf.parse((String) inValue);
				if (d != null) {
					return d;
				}
			}
		}

		// TODO fill with more possibilites
		return inDefault;
	}

	public static long getLexicalYYYYMMDDHHMMSSsss() {

		return getLexicalYYYYMMDDHHMMSSsss(getDate());
	}

	public static long getLexicalYYYYMMDDHHMMSSsss(long inDate) {

		return getLexicalYYYYMMDDHHMMSSsss(new Date(inDate));
	}

	public static Long getLexicalYYYYMMDDHHMMSSsss(Date inDate) {

		return inDate == null ? null : Long.parseLong(format(DATE_YYYYMMDDHHMMSSsss, inDate));
	}

	public static boolean isParseable(String inFormat, String inDate) {

		try {

			return inFormat != null && inDate != null && parse(inFormat, inDate) != null;

		} catch (final Exception e) {
			return false;
		}
	}

	public static Date max(Date... inDates) {

		if (inDates == null) {
			return null;
		}

		Date theMax = null;

		for (final Date aDate : inDates) {

			if (theMax == null && aDate != null) {
				theMax = aDate;
			} else if (theMax != null && aDate != null && aDate.getTime() > theMax.getTime()) {
				theMax = aDate;
			}
		}

		return theMax;
	}

	public static Date min(Date... inDates) {

		if (inDates == null) {
			return null;
		}

		Date theMin = null;

		for (final Date aDate : inDates) {

			if (theMin == null && aDate != null) {
				theMin = aDate;
			} else if (theMin != null && aDate != null && aDate.getTime() < theMin.getTime()) {
				theMin = aDate;
			}
		}

		return theMin;
	}
	
	public static boolean isAfter(Date inBase, Date inAfter) {
		
		if ( inBase == null || inAfter == null ) { return false; }
		
		return inBase.getTime() < inAfter.getTime();
	}
	
	public static boolean isAfterEqual(Date inBase, Date inAfterEqual) {
		
		if ( inBase == null || inAfterEqual == null ) { return false; }
		
		return inBase.getTime() <= inAfterEqual.getTime();
	}
	
	public static boolean isBefore(Date inBase, Date inBefore) {
		
		if ( inBase == null || inBefore == null ) { return false; }
		
		return inBase.getTime() > inBefore.getTime();
	}
	
	public static boolean isBeforeEqual(Date inBase, Date inBeforeEqual) {
		
		if ( inBase == null || inBeforeEqual == null ) { return false; }
		
		return inBase.getTime() >= inBeforeEqual.getTime();
	}

	public static Date setZeroOclock(Date inDate) {
		
		if ( inDate == null ) { return null; }
		
		return setZeroOclock( Timer.toCalendar( inDate ) ).getTime();
	}
	
	public static Date set235959999Oclock(Date inDate) {
		
		if ( inDate == null ) { return null; }
		
		return set2359599999Oclock( Timer.toCalendar( inDate ) ).getTime();
	}
	
	public static class Stopwatch {
		
		private List<Long> times = new ArrayList<Long>(1);
		
		private Long startTime = null;
		
		public Stopwatch() {
			
			this(true);
		}
		
		public Stopwatch(boolean inStart) {
			
			startTime = !inStart ? null : System.currentTimeMillis(); 
		}
		
		public void start() {
			synchronized (this) {
				startTime = startTime != null ? startTime : System.currentTimeMillis();
			}
		}
		
		public long getParttime() {
			
			if ( startTime == null ) { return 0; }

			long theTime;
			
			synchronized (this) {
				theTime = stop();
				start();
			}
			
			return theTime;
		}
		
		public long stop() {

			if ( startTime == null ) { return 0; }

			long theResult;
			
			synchronized (this) {
				
				times.add( theResult = System.currentTimeMillis()-startTime );
				startTime = null;
			}

			return theResult;
		}
		
		public List<Long> getTimes() {
			
			synchronized ( this ) {
				
				return new ArrayList<Long>( times );
			}
		}
		
		public int getCounts() {
			synchronized ( this ) {
				return times.size();
			}
		}
		
		public long getTimeSum() {
			
			long theSum = 0;
			
			synchronized ( this ) {
				
				for( Long aTime : times ) {
					
					theSum += aTime; 
				}
			}
			
			return theSum;
		}
		
		public Double getTimeAverage() {
			
			synchronized (this) {
				
				return times.isEmpty() ? null : getTimeSum() / (double)times.size();
			}
		}
	}

	public static Stopwatch newStopwatch(boolean inStart) {
		return new Stopwatch(inStart);
	}

	public static String getMillisAsHex() {
		return Long.toHexString( getMillis() );
	}

	public static Integer getDayOfMonth(Date inDate) {
		return inDate == null ? null : toCalendar( inDate ).get( Calendar.DAY_OF_MONTH );
	}
	public static Integer getMonth(Date inDate) {
		return inDate == null ? null : toCalendar( inDate ).get( Calendar.MONTH ) + 1;
	}
	public static Integer getYear(Date inDate) {
		return inDate == null ? null : toCalendar( inDate ).get( Calendar.YEAR );
	}
	
}

