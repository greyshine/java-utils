package de.greyshine.utils;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.greyshine.utils.deprecated.Timer;


/**
 * <SECS> <MINS> <HOURS> <MONTHS> <MONTHDAYS> <WEEKDAYS> <YEARS>
 */
public class Cron {

	private static final Log LOG = LogFactory.getLog(Cron.class);

	//final static String REGEX_JANxDEC = "(jan|feb|mar|may|jun|jul|aug|sep|oct|sep|nov|dec)";
	final static String REGEX_1x7 = "([1-7])";
	final static String REGEX_1x7x5NTH = "[1-7]#[1-5]";
	//final static String REGEX_MONxSUN = "(mon|tue|wed|thu|fri|sat|sun)(#[1-5])?";
	final static String REGEX_1x12 = "(0?[0-9]|1[012])";
	final static String REGEX_0x23 = "([01]?[0-9]|2[0-3])";
	final static String REGEX_0x59 = "[0-5]?[0-9]";
	final static String REGEX_1x31 = "([012]?[0-9]|3[01])";
	final static String REGEX_L0x30 = "[Ll](\\-([012]?[0-9]|30))?";
	final static String REGEX_YEAR = "[0-9]{4}";
	final static String REGEX_WILDCARDS = "[\\?\\*]";

	final static String REGEX_0x59_INTERVAL = REGEX_0x59 + "/" + REGEX_0x59;
	final static String REGEX_0x59_RANGE = REGEX_0x59 + "\\-" + REGEX_0x59;
	final static String REGEX_0x59_LISTITEM = "(" + REGEX_0x59 + "|" + REGEX_0x59_RANGE + "|" + REGEX_0x59_INTERVAL + ")";
	final static String REGEX_0x59_LIST = REGEX_0x59_LISTITEM + "(\\," + REGEX_0x59_LISTITEM + ")*";

	final static String REGEX_0x23_INTERVAL = REGEX_0x23 + "/" + REGEX_0x23;
	final static String REGEX_0x23_RANGE = REGEX_0x23 + "\\-" + REGEX_0x23;
	final static String REGEX_0x23_LISTITEM = "(" + REGEX_0x23 + "|" + REGEX_0x23_RANGE + "|" + REGEX_0x23_INTERVAL + ")";
	final static String REGEX_0x23_LIST = REGEX_0x23_LISTITEM + "(\\," + REGEX_0x23_LISTITEM + ")*";

	final static String REGEX_1x31_INTERVAL = REGEX_1x31 + "/" + REGEX_1x31;
	final static String REGEX_1x31_RANGE = REGEX_1x31 + "\\-" + REGEX_1x31;
	final static String REGEX_1x31_LISTITEM = "(" + REGEX_1x31 + "|" + REGEX_1x31_RANGE + "|" + REGEX_1x31_INTERVAL + "|" + REGEX_L0x30 + ")";
	final static String REGEX_1x31_LIST = REGEX_1x31_LISTITEM + "(\\," + REGEX_1x31_LISTITEM + ")*";

	final static String REGEX_1x12_INTERVAL = REGEX_1x12 + "/" + REGEX_1x12;
	final static String REGEX_1x12_RANGE = REGEX_1x12 + "\\-" + REGEX_1x12;
	final static String REGEX_1x12_LISTITEM = "(" + REGEX_1x12 + "|" + REGEX_1x12_RANGE + "|" + REGEX_1x12_INTERVAL + ")";
	final static String REGEX_1x12_LIST = REGEX_0x23_LISTITEM + "(\\," + REGEX_0x23_LISTITEM + ")*";

	final static String REGEX_1x7_INTERVAL = REGEX_1x7 + "/" + REGEX_1x7;
	final static String REGEX_1x7_RANGE = REGEX_1x7 + "\\-" + REGEX_1x7;
	final static String REGEX_1x7_LISTITEM = "(" + REGEX_1x7 + "|" + REGEX_1x7_RANGE + "|" + REGEX_1x7_INTERVAL + "|" + REGEX_1x7x5NTH + ")";
	final static String REGEX_1x7_LIST = REGEX_1x7_LISTITEM + "(\\," + REGEX_1x7_LISTITEM + ")*";

	final static String REGEX_YEAR_INTERVAL = REGEX_YEAR + "/[1-9][0-9]+";
	final static String REGEX_YEAR_RANGE = REGEX_YEAR + "\\-" + REGEX_YEAR;
	final static String REGEX_YEAR_LISTITEM = "(" + REGEX_YEAR + "|" + REGEX_YEAR_RANGE + "|" + REGEX_YEAR_INTERVAL + ")";
	final static String REGEX_YEAR_LIST = REGEX_YEAR_LISTITEM + "(\\," + REGEX_YEAR_LISTITEM + ")*";

	final static String REGEX_SECONDS = "(" + REGEX_WILDCARDS + "|(" + REGEX_0x59_LIST + "))";
	final static String REGEX_MINUTES = "(" + REGEX_WILDCARDS + "|(" + REGEX_0x59_LIST + "))";
	final static String REGEX_HOURS = "(" + REGEX_WILDCARDS + "|(" + REGEX_0x23_LIST + "))";
	final static String REGEX_DAYS = "(" + REGEX_WILDCARDS + "|(" + REGEX_1x31_LIST + "))";
	final static String REGEX_WEEKDAYS = "(" + REGEX_WILDCARDS + "|(" + REGEX_1x7_LIST + "))";
	final static String REGEX_MONTHS = "(" + REGEX_WILDCARDS + "|(" + REGEX_1x12_LIST + "))";
	final static String REGEX_YEARS = "(" + REGEX_WILDCARDS + "|(" + REGEX_YEAR_LIST + "))";
	final static String REGEX = REGEX_SECONDS + "(\\s" + REGEX_MINUTES + "(\\s" + REGEX_HOURS + "(\\s" + REGEX_DAYS + "(\\s" + REGEX_WEEKDAYS + "(\\s" + REGEX_MONTHS + "(\\s" + REGEX_YEARS + ")?)?)?)?)?)?";

	public static final GregorianCalendar GREGORIAN_CALENDAR = new GregorianCalendar();
	public static final int[] EMPTY_ARRAY_INTS = new int[0];
	public static final List<Integer> EMPTY_LIST_INTS = Collections.emptyList();
	public static final Set<Integer> EMPTY_SET_INTS = Collections.emptySet();

	/**
	 * any date after the 31.DEC.<code>veryMaxYear</code> is considered as
	 * 'unreachable'
	 */
	public int veryMaxYear = 9999;
	private List<Integer> currentMonthDays = new ArrayList<Integer>(31);
	private final String cronExpression;

	// all are real values
	int[] seconds;
	int[] minutes;
	int[] hours;
	// L-Days are L=0, L-1=-1, L-2=-2, ...
	int[] days;
	int[] months;
	int[] weekdays;
	int[] nthWeekdays;
	int[] years;

	//
	private final GregorianCalendar c = new GregorianCalendar();

	public Cron(String inTrigger) {

		this(inTrigger, null);
	}

	public Cron(String inExpression, Integer inVeryMaxYear) {
		
		veryMaxYear = inVeryMaxYear == null ? 99999 : inVeryMaxYear;

		//		System.out.println("\n");
		//		System.out.println(inExpression);
		//		System.out.println("-------------------------------");

		cronExpression = inExpression;
		inExpression = convertNamesToNumbers(inExpression);
		
		if (!(inExpression == null ? "" : inExpression.trim()).matches(REGEX)) {

			throw new IllegalArgumentException("Bad cron: " + (inExpression == null ? "" : inExpression));
		}

		final String[] theParts = inExpression.toLowerCase().trim().split(" ", -1);

		parseSeconds(theParts[0]);
		if (theParts.length > 1) {
			parseMinutes(theParts[1]);
		}
		if (theParts.length > 2) {
			parseHours(theParts[2]);
		}
		if (theParts.length > 3) {
			parseDays(theParts[3]);
		}
		if (theParts.length > 4) {
			parseWeekdays(theParts[4]);
		}
		if (theParts.length > 5) {
			parseMonths(theParts[5]);
		}
		if (theParts.length > 6) {
			parseYears(theParts[6]);
		}

		//		System.out.println("SEC: " + Arrays.toString(seconds));
		//		System.out.println("MIN: " + Arrays.toString(minutes));
		//		System.out.println("HRS: " + Arrays.toString(hours));
		//		System.out.println("DYS: " + Arrays.toString(days));
		//		System.out.println("WDS: " + Arrays.toString(weekdays));
		//		System.out.println("W#N: " + Arrays.toString(nthWeekdays));
		//		System.out.println("MON: " + Arrays.toString(months));
		//		System.out.println("YRS: " + Arrays.toString(years));
	}

	private void parseSeconds(String inExpression) {

		final Results r = new Results(0, 59);
		parse(r, inExpression);
		seconds = r.getValues();
	}

	private void parseMinutes(String inExpression) {

		final Results r = new Results(0, 59);
		parse(r, inExpression);
		minutes = r.getValues();
	}

	private void parseHours(String inExpression) {

		final Results r = new Results(0, 23);
		parse(r, inExpression);
		hours = r.getValues();
	}

	private void parseMonths(String inExpression) {

		final Results r = new Results(1, 12);
		parse(r, inExpression);
		months = r.getValues();
	}

	private void parseWeekdays(String inExpression) {

		final Results r = new Results(1, 7);
		parse(r, inExpression);
		weekdays = r.getValues();
	}

	private void parseYears(String inExpression) {

		final Results r = new Results(0, veryMaxYear);
		parse(r, inExpression);
		years = r.getValues();

	}

	private void parseDays(String inExpression) {

		final Results r = new Results(1, 31);
		parse(r, inExpression);
		days = r.getValues();
	}

	private void parse(Results r, String inExpression) {

		inExpression = inExpression.trim();

		if ("*".equals(inExpression)) {

			r.adds(r.firstValue, r.lastValue, 1);
			return;

		} else if (inExpression.contains(",")) {

			for (final String aPart : inExpression.split(",", -1)) {

				parse(r, aPart);
			}

			return;

		} else if (inExpression.matches(REGEX_L0x30)) {

			final int theL = ("l".equals(inExpression) ? 0 : Integer.parseInt(inExpression.substring(1)));
			r.add(theL);

		} else if (inExpression.contains("/")) {

			final int theValue = Integer.parseInt(inExpression.substring(0, inExpression.indexOf('/')));
			final int theInterval = Integer.parseInt(inExpression.substring(inExpression.indexOf('/') + 1));

			r.adds(theValue, r.lastValue, theInterval);
			return;

		} else if (inExpression.contains("-")) {

			final int theValue = Integer.parseInt(inExpression.substring(0, inExpression.indexOf('-')));
			final int theMax = Integer.parseInt(inExpression.substring(inExpression.indexOf('-') + 1));

			r.adds(theValue, theMax, 1);
			return;

		} else if (inExpression.contains("#")) {

			r.add(Character.getNumericValue(inExpression.charAt(0)) * 10 + Character.getNumericValue(inExpression.charAt(2)));
		}

		else if (!"?".equals(inExpression)) {

			r.add(Integer.parseInt(inExpression));
		}
	}

	private boolean isAfterLast() {

		return years != null && c.get(Calendar.YEAR) > years[years.length - 1] || c.get(Calendar.YEAR) > veryMaxYear;
	}

	public Long getTimeToNextInvocation() {

		final Long theNextInvocation = getNextInvocation();
		return theNextInvocation == null ? null : theNextInvocation;
	}

	private boolean forwardYear() {

		if (years == null) {

			return false;

		}

		final int theNowYear = getNow(c, Calendar.YEAR);
		final Integer theNext = fetchEqualOrNext(years, theNowYear);
		final int theYearsToAdd = (theNext == null ? veryMaxYear + 1 : theNext) - theNowYear;

		if (theYearsToAdd > 0) {

			c.set(Calendar.MONTH, Calendar.JANUARY);
			c.set(Calendar.DAY_OF_MONTH, 1);
			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);

			c.add(Calendar.YEAR, theYearsToAdd);

			currentMonthDays.clear();
			
			//LOG.debug("> corrected year: " + theNowYear + " to " + theNext + " > " + theYearsToAdd + ": " + c.getTime());
			
			return true;
		}

		return false;
	}

	private boolean forwardMonths() {

		if ( months == null ) { return false; }
		
		final int theNowMonth = getNow(c, Calendar.MONTH);
		final Integer theNext = fetchEqualOrNext(months, theNowMonth);
		final int theMonthsToAdd = theNext != null ? theNext - theNowMonth : 12 - theNowMonth + months[0];
		
		if ( theMonthsToAdd != 0 ) {
			
			c.set(Calendar.DAY_OF_MONTH, 1);
			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);
			
			c.add(Calendar.MONTH, theMonthsToAdd);
			
			currentMonthDays.clear();
			
			LOG.debug("> corrected month: " + theNowMonth + " to " + theNext + " > " + theMonthsToAdd + ": " + c.getTime());
		}
		
		final boolean isYearChange = theNext == null;

		return isYearChange;
	}

	private boolean forwardDays() {

		final int theCurrentMonth = c.get(Calendar.MONTH);
		
		if (currentMonthDays.isEmpty()) {

			LOG.debug("> no days in month " + getNow(c, Calendar.MONTH) + "." + getNow(c, Calendar.YEAR));
			
			c.set(Calendar.DAY_OF_MONTH, 1);
			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);

			c.set(Calendar.MONTH, theCurrentMonth + (theCurrentMonth != Calendar.DECEMBER ? 1 : -11));

			return true;
		}

		final int theNowDay = getNow(c, Calendar.DAY_OF_MONTH);
		final Integer theNext = fetchEqualOrNext(currentMonthDays, theNowDay);
		
		if (theNext == null) {

			LOG.debug(">a> day in month is past");

			c.set(Calendar.DAY_OF_MONTH, 1);
			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);

			c.add(Calendar.MONTH, 1);
			
			currentMonthDays.clear();

			return true;
		}

		// from here no change in month will occur!
		final int theDaysToAdd = theNext - theNowDay;
		if (theDaysToAdd != 0) {

			c.set(Calendar.HOUR_OF_DAY, 0);
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);

			c.add(Calendar.DAY_OF_MONTH, theDaysToAdd);

			LOG.debug("> corrected days " + theNowDay + " to " + theNext + " > " + theDaysToAdd);
		}

		return false;
	}
	
	private boolean forwardHours() {
		
		if ( hours == null ) { return false; }
		
		final int theNowHour = c.get(Calendar.HOUR_OF_DAY);
		final Integer theNext = fetchEqualOrNext(hours, theNowHour);
		final int theHoursToAdd = theNext != null ? theNext - theNowHour : 24 - theNowHour + hours[0];
		
		if (theHoursToAdd > 0) {

			final int theNowDay = c.get(Calendar.DAY_OF_MONTH);
			final int theNowMonth = c.get(Calendar.MONTH);
			
			c.set(Calendar.MINUTE, 0);
			c.set(Calendar.SECOND, 0);
			
			c.add(Calendar.HOUR_OF_DAY, theHoursToAdd);
			
			LOG.debug("> corrected hours: " + theNowHour + " to " + theNext + " > " + theHoursToAdd + " : " + c.getTime());

			if (theNowMonth != c.get(Calendar.MONTH)) {
				
				currentMonthDays.clear();
				return true;
			}
			
			return theNext == null || theNowDay != c.get(Calendar.DAY_OF_MONTH);
		}
		
		return  false;
	}
	
	private boolean forwardMinutes() {
		
		if ( minutes == null ) { return false; }
		
		final int theNowMinute = c.get(Calendar.MINUTE);
		final Integer theNext = fetchEqualOrNext(minutes, theNowMinute);
		final int theMinutesToAdd = theNext != null ? theNext - theNowMinute : 60 - theNowMinute + minutes[0] ;
		
		if (theMinutesToAdd > 0) {
			
			final int theNowDay = c.get(Calendar.DAY_OF_MONTH);
			final int theNowMonth = c.get(Calendar.MONTH);
			
			c.set(Calendar.SECOND, 0);
			
			c.add(Calendar.MINUTE, theMinutesToAdd);
			
			LOG.debug("> corrected minutes: " + theNowMinute + " to " + theNext + " > " + theMinutesToAdd + " : " + c.getTime());
			
			if (theNowMonth != c.get(Calendar.MONTH)) {
				
				currentMonthDays.clear();
				return true;
			}
			
			return theNext == null || theNowDay != c.get(Calendar.DAY_OF_MONTH);
		}
		
		return  false;
	}
	
	private boolean forwardSeconds() {
		
		if ( seconds == null ) { return false; }
		
		final int theNowSecond = c.get(Calendar.SECOND);
		final Integer theNext = fetchEqualOrNext(seconds, theNowSecond);
		final int theSecondsToAdd = theNext != null ? theNext - theNowSecond : 60 - theNowSecond + seconds[0] ;
		
		if (theSecondsToAdd > 0) {
			
			final int theNowDay = c.get(Calendar.DAY_OF_MONTH);
			final int theNowMonth = c.get(Calendar.MONTH);
			
			c.add(Calendar.SECOND, theSecondsToAdd);
			
			//LOG.debug("> corrected seconds: " + theNowSecond + " to " + theNext + " > " + theSecondsToAdd + " : " + c.getTime());
			
			if (theNowMonth != c.get(Calendar.MONTH)) {
				
				currentMonthDays.clear();
				return true;
			}
			
			return theNext == null || theNowDay != c.get(Calendar.DAY_OF_MONTH);
		}
		
		return  false;
	}

	public Long getNextInvocation() {
		
		c.setTimeInMillis(Timer.getMillis());
		// forward to next second
		c.add(Calendar.MILLISECOND, 1000 - c.get(Calendar.MILLISECOND));

		//LOG.debug("next invocation evaluation from now:" + c.getTime() + " ...");

		while (!isAfterLast()) {

			//LOG.debug("loop: " + c.getTime());

			if (forwardYear()) {

				continue;
			}

			if (forwardMonths()) {

				continue;
			}

			if (currentMonthDays.isEmpty()) {

				collectMonthDays();
			}
			
			if ( forwardDays() ) {

				continue;
			}
			
			if ( forwardHours() ) {
				
				continue;
			}
			
			if ( forwardMinutes() ) {
				
				continue;
			}
			
			if ( forwardSeconds() ) {
				
				continue;
			}

			// all times are properly setup and no re-loop has occured
			break;
		}

		return isAfterLast() ? null : c.getTimeInMillis();
	}


	private int getNow(Calendar inC, int inCField) {

		int theCorrection = 0;
		
		switch (inCField) {
		case Calendar.DAY_OF_WEEK:

			// SUNDAY=1; but must be 7, MONDAY = 2; must be 1 , ...
			theCorrection = inC.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY ? 6 : -1;
			break;

		case Calendar.MONTH:
			
			theCorrection = 1;
			break;

		case Calendar.DAY_OF_WEEK_IN_MONTH:

			int theNth = 0;

			final Calendar c2 = Calendar.getInstance();
			c2.setTimeInMillis(inC.getTimeInMillis());

			while (c2.get(Calendar.MONTH) == inC.get(Calendar.MONTH)) {

				theNth++;
				c2.add(Calendar.DAY_OF_MONTH, -7);
			}

			return (getNow(inC, Calendar.DAY_OF_WEEK) * 10) + theNth;
		}

		return inC.get(inCField) + theCorrection;
	}

	private void collectMonthDays() {

		currentMonthDays.clear();

		final Calendar c = Calendar.getInstance();
		c.set(Calendar.YEAR, c.get(Calendar.YEAR));
		c.set(Calendar.MONTH, c.get(Calendar.MONTH));
		c.set(Calendar.DAY_OF_MONTH, 1);

		final int theCurrentMonth = c.get(Calendar.MONTH);

		final int theLastDay = getDaysInMonth(c);
		//final int theNthWeekday = 1;

		while (theCurrentMonth == c.get(Calendar.MONTH)) {

			final int theDay = c.get(Calendar.DAY_OF_MONTH);
			final int theLday = theDay - theLastDay;
			final int theWeekday = getWeekday(c);
			final int theNthWeekday = (theDay / 7) + 1;
			final int theNth = (theWeekday * 10) + theNthWeekday;

			boolean isCorrectDay = isOneOf(days, theDay) || isOneOf(days, theLday);
			isCorrectDay = isCorrectDay && isOneOf(weekdays, theWeekday);
			isCorrectDay = isCorrectDay && isOneOf(nthWeekdays, theNth);

			//System.out.println("collectDays :: " + theDay + " (L" + theLday + ") , wd=" + theWeekday + "#" + theNthWeekday + " (" + theNth + ") - " + isCorrectDay + " :: " + c.getTime());

			if (isCorrectDay) {

				//LOG.debug("collectDays :: " + theDay + " (L" + theLday + ") , wd=" + theWeekday + "#" + theNthWeekday + " (" + theNth + ") - " + isCorrectDay + " :: " + c.getTime());
				currentMonthDays.add(theDay);
			}

			c.add(Calendar.DAY_OF_MONTH, 1);

		}

		//LOG.debug("daysWithinMonth " + (c.get(Calendar.MONTH) + 1) + "." + c.get(Calendar.YEAR) + ": " + currentMonthDays);
	}

	private static int getDaysInMonth(Calendar inCalendar) {

		switch (inCalendar.get(Calendar.MONTH)) {

		case Calendar.JANUARY:
		case Calendar.MARCH:
		case Calendar.MAY:
		case Calendar.JULY:
		case Calendar.AUGUST:
		case Calendar.OCTOBER:
		case Calendar.DECEMBER:
			return 31;
		case Calendar.APRIL:
		case Calendar.JUNE:
		case Calendar.SEPTEMBER:
		case Calendar.NOVEMBER:
			return 30;
		case Calendar.FEBRUARY:
			return GREGORIAN_CALENDAR.isLeapYear(inCalendar.get(Calendar.YEAR)) ? 29 : 28;

		default:
			throw new IllegalStateException("Illegal gregorian month, must be between " + Calendar.JANUARY + "-" + Calendar.DECEMBER + " [month=" + inCalendar.get(Calendar.MONTH) + "]");
		}
	}

	private static String convertNamesToNumbers(String inName) {

		if (inName == null) {

			return null;
		}

		final StringBuilder sb = new StringBuilder();

		final Matcher m = Pattern.compile("[a-z]{3}").matcher(inName);

		int i = 0;

		while (m.find()) {

			sb.append(inName.substring(i, m.start()));

			i = m.start() + 3;

			switch (inName.substring(m.start(), i)) {

			case "jan":
			case "mon":
				sb.append(1);
				break;
			case "feb":
			case "tue":
				sb.append(2);
				break;
			case "mar":
			case "wed":
				sb.append(3);
				break;
			case "apr":
			case "thu":
				sb.append(4);
				break;
			case "may":
			case "fri":
				sb.append(5);
				break;
			case "jun":
			case "sat":
				sb.append(6);
				break;
			case "jul":
			case "sun":
				sb.append(7);
				break;
			case "aug":
				sb.append(8);
				break;
			case "sep":
				sb.append(9);
				break;
			case "oct":
				sb.append(10);
				break;
			case "nov":
				sb.append(11);
				break;
			case "dec":
				sb.append(12);
				break;

			default:
				throw new IllegalArgumentException(inName.substring(m.start(), i));

			}

		}

		sb.append(inName.substring(i));

		return sb.toString();
	}

	/**
	 * Honors my real life world; week starts with MONDAY=1 and ends with
	 * SUNDAY=7
	 * 
	 * @param inC
	 * @return
	 */
	public static int getWeekday(Calendar inC) {

		int theWd = inC.get(Calendar.DAY_OF_WEEK) - 1;
		theWd = theWd == 0 ? 7 : theWd;
		//System.out.println(theWd + " (" + inC.get(Calendar.DAY_OF_WEEK) + ") > " + inC.getTime());
		return theWd;
	}

	private static boolean isOneOf(int[] inValues, int inValue) {

		//System.out.println(inValue + " in " + (inValues == null ? null : Arrays.toString(inValues)) + " ...");

		if (inValues == null) {

			return true;
		}

		//System.out.print(inValue + " in " + Arrays.toString(inValues) + " - ");

		for (final int v : inValues) {

			if (v == inValue) {
				//System.out.println(true);
				return true;
			}
		}

		//System.out.println(false);
		return false;
	}

	private static Integer fetchEqualOrNext(int[] inValues, int inValue) {

		if (inValues == null) {
			return inValue;
		}

		for (final int v : inValues) {

			if (v >= inValue) {
				return v;
			}
		}

		return null;
	}
	
	private static Integer fetchEqualOrNext(List<Integer> inValues, int inValue) {
		
		if (inValues == null) {
			return inValue;
		}
		
		for (final int v : inValues) {
			
			if (v >= inValue) {
				return v;
			}
		}
		
		return null;
	}

	static class Results {

		final List<Integer> values = new ArrayList<Integer>();
		final int firstValue, lastValue;

		public Results(int firstValue, int lastValue) {

			this.firstValue = firstValue;
			this.lastValue = lastValue;
		}

		void adds(int inStart, int inEnd, int inInterval) {

			inEnd = Math.min(inEnd, lastValue);

			while (inStart <= inEnd) {

				add(inStart);
				inStart += inInterval;
			}
		}

		void add(Integer inValue) {

			if (inValue == null || values.contains(inValue)) {

				return;
			}

			values.add(inValue);
		}

		int[] getValues() {

			if (values.isEmpty()) {

				return null;
			}

			Collections.sort(values);

			final int[] r = new int[values.size()];
			for (int i = 0; i < r.length; i++) {

				r[i] = values.get(i);
			}

			return r;
		}
	}

	@Override
	public String toString() {
		return "Cron [cron=" + cronExpression + ", timeToNextInvocation=" + getTimeToNextInvocation() + ", veryMaxYear=" + veryMaxYear + "]";
	}

}
