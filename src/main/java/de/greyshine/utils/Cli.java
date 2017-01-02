package de.greyshine.utils;

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import de.greyshine.utils.deprecated.Utils;

public abstract class Cli {

	public final boolean isVerbose;
	public final boolean isQuiet;

	protected boolean isTerminate1 = false;

	/**
	 * contains no null values, uses "" instead
	 */
	protected final String[] args;

	protected Cli(String[] inArgs) {
		
		args = inArgs == null ? new String[0] : inArgs;

		for (int i = 0; i < args.length; i++) {

			args[i] = Utils.trimToEmpty(args[i]);
		}
		
		isVerbose = isParam("-verbose", true);
		isQuiet = isParam("-quiet", true);

		try {
			construct();

			if (!isTerminate1) {

				main();
			}

		} catch (final Exception e) {

			isTerminate1 = true;
			throw e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
		}
	}
	
	public void construct() {
	};

	protected static String urlEncode(String inValue) {

		try {
			return inValue == null ? "" : URLEncoder.encode(inValue, "UTF-8");
		} catch (final UnsupportedEncodingException e) {

			throw new RuntimeException(e);
		}
	}

	public void printErr(Object inObject, boolean isTerminate1, boolean inVerbose) {

		this.isTerminate1 = this.isTerminate1 || isTerminate1;

		if (inObject == null || isQuiet) {
			return;
		} else if (!isVerbose && inVerbose) {
			return;
		}

		System.err.println(inObject);
	}

	public boolean isParam(String inName, boolean inIgnoreCase) {

		for (final String a : args) {

			if (a == null) {

				continue;

			} else if (inIgnoreCase && a.trim().equalsIgnoreCase(inName)) {

				return true;
			
			} else if (!inIgnoreCase && a.trim().equals(inName)) {
				
				return true;
			}
		}

		return false;

	}

	public int getArgIndex(String inName, boolean inIgnoreCase) {

		for (int i = 0, l = args.length; i < l; i++) {

			if (inIgnoreCase && args[i].toLowerCase().startsWith(inName.toLowerCase())) {

				return i;

			} else if (!inIgnoreCase && args[i].startsWith(inName)) {

				return i;
			}
		}

		return -1;
	}

	public abstract void main() throws Exception;

	public void terminate() {

		if (isTerminate1) {
			System.exit(1);
		}
	}

	public abstract String getUsage();

	protected final void displayUsage() {
		displayUsage(null);
	}

	protected final void displayUsage(String inMessage) {

		inMessage = Utils.trimToEmpty(inMessage);

		if (!inMessage.isEmpty()) {

			System.out.println(inMessage);
		}

		final String theUsage = Utils.trimToEmpty(getUsage());

		if (!theUsage.isEmpty()) {

			System.out.println(theUsage);
		}
	}

	public static String slice_(String inBeginn, String[] inArgs) {

		for (final String a : inArgs) {

			if (a != null && inBeginn != null && a.startsWith(inBeginn)) {

				return a.substring(inBeginn.length());
			}

		}

		return null;
	}
	
	public ParameterBuilder getParameterBuilder() {

		return new ParameterBuilder();
	}

	public List<ParameterBuilder> getBuildersBeginAtLast(String inQuitRegex, Integer inLastIndex) {

		final List<ParameterBuilder> theResult = new ArrayList<ParameterBuilder>(0);

		final Pattern theQuitPattern = Utils.compilePattern(inQuitRegex);

		if (theQuitPattern == null) {
			return theResult;
		}

		for (int i = args.length - 1; i > 0; i--) {

			if (inLastIndex != null && i < inLastIndex) {

				break;

			} else if (Utils.isMatch(args[i], theQuitPattern)) {

				break;
			}
			
			theResult.add(0, new ParameterBuilder().index(i, false));
		}

		return theResult;
	}

	public class ParameterBuilder {

		private int index = -1;
		private final List<String> values = new ArrayList<String>(0);
		
		private ParameterBuilder() {

		}

		public ParameterBuilder index(int i, boolean inArgToValue) {

			values.clear();
			index = i < 0 || i >= args.length || args.length == 0 ? -1 : i;

			return !inArgToValue ? this : argToValue();
		}

		public ParameterBuilder findPrefixed(String inPrefix, boolean inArgToValue) {

			return findPrefixed(0, inPrefix, inArgToValue);
		}

		public ParameterBuilder findPrefixed(int inStartIndex, String inPrefix, boolean inArgToValue) {

			if (inPrefix == null || inStartIndex < 0) {
				return this;
			}

			for (int i = inStartIndex; i < args.length; i++) {

				if (args[i].startsWith(inPrefix)) {

					return index(i, inArgToValue);
				}
			}

			return this;
		}

		public ParameterBuilder unwrapValues(final Character inWrapChar) {

			for (int i = 0; i < values.size(); i++) {

				values.set(i, Utils.unwrap(values.get(i), inWrapChar, true));
			}

			return this;
		}
		


		public IParameter build() {

			return index < 0 || index >= args.length ? null : new IParameter() {

				final int index = Cli.ParameterBuilder.this.index;
				final List<String> values = new ArrayList<String>(Cli.ParameterBuilder.this.values);
				
				@Override
				public String getArg() {
					return Utils.trimToEmpty(args[this.index]);
				}

				@Override
				public int getValuesSize() {
					return this.values.size();
				}

				@Override
				public List<String> getValues() {
					return this.values;
				}

				@Override
				public Integer getValueInteger() {
					return Utils.parseInteger(this.getValue());
				}

				@Override
				public File getValueFile() {
					return Utils.getCanonicalPathQuietly(this.getValue(), true);
				}

				@Override
				public Boolean getValueBoolean() {
					return Utils.parseBoolean(this.getValue());
				}

				@Override
				public String getValue() {
					return Utils.defaultIfBlank(this.getValue(0), getArg());
				}

				@Override
				public String getValue(int inIndex) {
					return Utils.trimToEmpty(inIndex < 0 || inIndex >= values.size() ? "" : values.get(inIndex));
				}

				@Override
				public boolean isValueBlank() {
					return Utils.isBlank(this.getValue());
				}

				@Override
				public boolean isValueNotBlank() {
					return Utils.isNotBlank(this.getValue());
				}

				@Override
				public int getIndex() {
					return this.index;
				}

				@Override
				public String toString() {

					return "Parameter [index=" + getIndex() + ", arg=" + getArg() + ", values(" + values.size() + ")=" + values + "]";
				}
			};

		}

		public ParameterBuilder splitValues(String inSplit) {

			final List<String> theValues = new ArrayList<String>(values.size());

			for (final String aValue : values) {

				theValues.addAll(Utils.split(aValue, inSplit));
			}

			values.clear();
			values.addAll(theValues);

			return this;
		}

		/**
		 * substring(offset) of arg[index] position
		 * 
		 * @param inIndex
		 * @param inOffset
		 * @return
		 */
		public ParameterBuilder substringValue(int inIndex, int inOffset) {
			
			if ( inIndex < 0 || inIndex >= values.size() || values.get( inIndex ) == null) { return this; }
			
			final String theValue = values.get( inIndex );
			
			if (theValue.length() < inOffset) {
				return this;
			}

			values.set(inIndex, theValue.substring(inOffset));
			
			return this;
		}

		public ParameterBuilder appendArgValues(String inQuitRegex) {

			final Pattern thePattern = Utils.compilePattern(inQuitRegex);

			if (thePattern == null || index == -1) {

				return this;
			}

			for (int i = index + 1; i < args.length; i++) {

				if (Utils.isMatch(args[0], thePattern)) {

					break;
				}
				
				values.add(args[i]);
			}

			return this;
		}

		public ParameterBuilder appendArgValues(int inLen) {
			
			if (inLen < 1 || index == -1) {
				return this;
			}
			
			for (int i = index; i < args.length && inLen > 0; i++, inLen--) {

				values.add(Utils.trimToEmpty(args[i]));
			}

			return this;
		}

		/**
		 * Makes the arg at the index become the only value
		 * @return
		 */

		public ParameterBuilder argToValue() {

			values.clear();
			values.add(index < 0 || index >= args.length ? null : args[index]);

			return this;
		}

		public ParameterBuilder splitValue(String inSplit) {

			return splitValue(0, inSplit);
		}

		public ParameterBuilder splitValue(int inIndex, String inSplit) {

			if (inIndex < 0 || inIndex >= values.size() || inSplit == null) {

				return this;
			}

			final String theValue = values.get(inIndex);
			values.remove(inIndex);
			values.addAll(inIndex, Utils.split(theValue, inSplit));

			return this;
		}

		public ParameterBuilder unwrapValue(int inIndex, char inWrapChar) {

			if (inIndex < 0 || inIndex >= values.size()) {
				return this;
			}

			values.set(inIndex, Utils.unwrap(values.get(inIndex), inWrapChar));

			return this;
		}

		public int getIndex() {

			return index;
		}

		public String getArg() {

			return Utils.trimToEmpty(args[this.index]);
		}
	}
	
	public interface IParameter {
		
		int getIndex();

		String getArg();

		List<String> getValues();
		String getValue();
		String getValue(int inIndex);
		Boolean getValueBoolean();
		Integer getValueInteger();
		File getValueFile();

		boolean isValueBlank();

		boolean isValueNotBlank();

		int getValuesSize();
	}
}
