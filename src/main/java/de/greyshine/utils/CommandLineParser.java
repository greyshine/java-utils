package de.greyshine.utils;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.regex.Pattern;

import de.greyshine.utils.CommandLineParser.Option;

/**
 * Helper to evaluate <tt>public static void main(String[] args)</tt>-Args
 */
public class CommandLineParser {

	private List<Option> options = new ArrayList<>();
	private List<SimpleArg> simpleArgs = new ArrayList<>(0);

	private String headerText;
	private String usageText;
	private String footerText;

	public class SimpleArg {
		
		private String name;
		private boolean isOptional = false;
		private boolean isMulti = false;
		private String description;
		
		private SimpleArg(String inName) {
			if ( inName == null ) throw new IllegalArgumentException("name must not be blank");
			name = Utils.trimToNull( inName );
			simpleArgs.add( this );
		}
		
		public SimpleArg multi() {
			for (SimpleArg simpleArg : simpleArgs) {
				if ( simpleArg.isMulti ) {
					throw new IllegalStateException("there has already bee a parameter with option multi: "+ simpleArg.name);
				}
			}
			
			this.isMulti = true;
			return this;
		}
		
		public SimpleArg optional() {
			isOptional = true;
			return this;
		}
		
		public SimpleArg description(String inDescription) {
			this.description = inDescription;
			return this;
		}
		
		public CommandLineParser done() {
			return CommandLineParser.this;
		}
		
	}
	
	public class Option {

		private String option;
		private String longoption;

		private String parameterName;
		private Pattern parameterPattern;
		
		private String description;

		/**
		 * Must this option be existing?
		 */
		private boolean isOptional = false;

		private Option(String inOption) {
			option = inOption.trim();
			options.add( this );
		}

		public Option optional() {
			return optional(true);
		}

		public Option optional(boolean isOptional) {
			this.isOptional = isOptional;
			return this;
		}

		public Option longoption(String inLongoption) {

			final String theLongoption = inLongoption == null ? "" : inLongoption.trim();
			if (theLongoption.isEmpty()) {
				return this;
			}

			final Option that = this;
			options.stream().forEach((o) -> {
				if (o != that && o.longoption.equals(inLongoption)) {
					throw new IllegalArgumentException("longoption '" + inLongoption + "'already used: " + o);
				}
			});

			longoption = theLongoption;
			return this;
		}

		public Option parameter(String inArgParameter) {
			if ( inArgParameter == null || inArgParameter.trim().isEmpty()) { throw new IllegalArgumentException("arg does not have a name"); }
			this.parameterName = inArgParameter.trim();
			return this;
		}

		public boolean isOptionParameterized() {
			return parameterName != null;
		}

		public Option description(String inDescription) {
			this.description = inDescription;
			return this;
		}

		@Override
		public String toString() {
			return "Option -" + option;
		}

		public CommandLineParser done() {
			return CommandLineParser.this;
		}

		public Option regex(String inRegex) {
			this.parameterPattern =  inRegex==null? null : Pattern.compile( inRegex );  
			return this;
		}
	}
	
	public SimpleArg simpleArg(String inName) {
		
		if ( Utils.isBlank( inName ) ) { return null; }
		
		return new SimpleArg(inName);
	}

	public Option option(String inOption) {
		return option( inOption, null );
	}
	
	public Option option(String inOption, String inLongoption) {
		
		if (inOption == null || (inOption = inOption.trim()).isEmpty()) {
			return null;
		}
		
		for (Option o : options) {
			
			if (o.option.equals(inOption)) {
				return o;
			}
		}
		
		return new Option(inOption).longoption(inLongoption);
	}

	public CommandLineParser usageText(String inUsageText) {
		usageText = "usage: " + (inUsageText == null ? "" : inUsageText);
		return this;
	}

	public CommandLineParser generateUsageText(String inCommand) {

		String usageLine = inCommand;
		for (Option anOption : options) {
			
			usageLine +=" ";
			if ( anOption.isOptional ) {
				usageLine += "[";
			}
			usageLine += "-"+ anOption.option;
			
			if ( anOption.parameterName != null ) {
				usageLine += " <"+ anOption.parameterName +">";
			}
			
			if ( anOption.isOptional ) {
				usageLine += "]";
			}
		}
		
		for( SimpleArg sa : simpleArgs ) {
			usageLine += " ";
			usageLine += "<"+sa.name+">";
			if ( sa.isMulti ) {
				usageLine+="...";
			}
		}
		
		String optionsBlock = "";

		final int theLengthOptions = options.stream().mapToInt((o) -> {

			// "-option"
			int w = 1 + o.option.length();
			// ", --longoption"
			w += (o.longoption == null ? 0 : 4 + o.longoption.length());
			
			w += ( !o.isOptionParameterized() ? 0 : o.parameterName.length() + 3);
			
			return w;

		}).max().orElse(0);

		int theLengthSimpleArgs = simpleArgs.stream().mapToInt((sa) -> {
			
			int len = sa.name.length();
			len += 2; // +2 because of "<name>"
			len += ( sa.isMulti ? 3 : 0 ); // beacause "..."
			return len;
			
		}).max().orElse(0);
		
		final int theLength = 2+Math.max( theLengthOptions , theLengthSimpleArgs);
		
		for (Option anOption : options) {
			
			String optionSubBlock = "-"+anOption.option;
			if (anOption.longoption != null) {
				optionSubBlock += ", --" + anOption.longoption;
			}
			
			if ( anOption.isOptionParameterized() ) {
				optionSubBlock += " <"+ anOption.parameterName +">";
			}

			while (optionSubBlock.length() < theLength) {
				optionSubBlock += " ";
			}

			boolean isFirstLine = true;
			for (String dLine : (anOption.description == null ? "" : anOption.description).split("\\n", -1)) {

				if (isFirstLine) {

					isFirstLine = false;

				} else {

					for (int i = 0; i < theLength; i++) {
						dLine = " " + dLine;
					}

					dLine = "\n" + dLine;
				}

				optionSubBlock += dLine;
			}

			optionsBlock += "\n" + optionSubBlock;
		}
		
		if ( !simpleArgs.isEmpty() ) {
			optionsBlock +="\n";
		}
		
		for( SimpleArg sa : simpleArgs ) {
			
			String optionSubBlock = "<"+ sa.name +">";
			
			if (sa.isMulti) {
				optionSubBlock += "...";
			}
			
			while( optionSubBlock.length() < theLength ) {
				optionSubBlock += " ";
			}
			
			boolean isFirstLine = true;
			for (String dLine : (sa.description == null ? "" : sa.description).split("\\n", -1)) {

				if (isFirstLine) {

					isFirstLine = false;

				} else {

					for (int i = 0; i < theLength; i++) {
						dLine = " " + dLine;
					}

					dLine = "\n" + dLine;
				}

				optionSubBlock += dLine;
			}
			
			optionsBlock += "\n" + optionSubBlock;
			
		}

		return usageText(usageLine + System.lineSeparator() + optionsBlock);
	}

	public CommandLineParser headerText(String inHeaderText) {
		headerText = inHeaderText;
		return this;
	}

	public CommandLineParser footerText(String inFooterText) {
		footerText = inFooterText;
		return this;
	}

	public String getHelp() {

		final StringBuilder sb = new StringBuilder();

		if (headerText != null) {
			sb.append(headerText);
			sb.append(System.lineSeparator());
		}

		if (usageText != null) {
			sb.append(usageText);
		}

		if (footerText != null) {
			sb.append(System.lineSeparator());
			sb.append(footerText);
		}

		return sb.toString().trim();
	}

	public void printHelp() {
		System.out.println(getHelp());
	}

	public Args parse(String[] inArgs) {
		return new Args(inArgs);
	}

	public Option lookupOption(String inOption) {
		for (Option option : options) {
			if ( option.option.equals( inOption ) ) {
				return option;
			}
		}
		return null;
	}
	
	public Option lookupLongoption(String inLongOption) {
		for (Option option : options) {
			if ( option.longoption != null && option.longoption.equals( inLongOption ) ) {
				return option;
			}
		}
		return null;
	}
	
	public class Args {

		private final String[] args;
		
		public Args(String[] args) {

			this.args = args == null ? new String[0] : args;

			for (int i = 0; i < args.length; i++) {
				this.args[i] = this.args[i] == null ? "" : this.args[i];
			}
		}
		
		public List<String> getArgs() {
			
			final List<String> theArgs = new ArrayList<>(0);
			
			for( String a : args ) {
				theArgs.add( a );
			}
			
			return theArgs;
		}

		/**
		 * 
		 * @return values after the last Option (and its parameter if existing)
		 */
		public List<String> getSimpleArgs() {

			// take last optional argument, fetch its index check if there might be an optional arg for that option
			// then take the last arguments as method result
			
			final List<String> vs = new ArrayList<>();
			
			for( int i = args.length-1 ; i >= 0 ; i-- ) {
				
				throw new UnsupportedOperationException("need to implement, no time right now");
				
			}
			
			return vs;
		}

		public List<File> getSimpleArgsAsFiles() {

			final List<File> vs = new ArrayList<>();

			getSimpleArgs().stream().filter(Utils::isNotBlank).map(File::new).forEach(vs::add);

			return vs;
		}
		
		public String getSimpleArg(int inIndex) {
			return Utils.getIndexedValueSafe( getSimpleArgs(), inIndex, null);			
		}
		
		public File getSimpleArgAsFile(int inIndex) {
			final String theValue = getSimpleArg(inIndex);
			return Utils.isBlank( theValue ) ? null : new File( theValue );
		}

		public int getOptionIndex(String inOption) {
			return getOptionIndex( lookupOption( inOption ) );
		}

		private int getOptionIndex(Option inOption) {
			
			if ( inOption == null ) { return -1; }
			
			final String o = "-" + inOption.option;
			final String lo = inOption.longoption == null ? null : "--" + inOption.longoption;
			
			for (int i = 0; i < args.length; i++) {
				if (o.equals(args[i])) {
					return i;
				} else if ( lo != null && lo.equals( args[i] ) ) {
					return i;
				}
			}
			
			return -1;
		}

		/**
		 * 
		 * @param inOption
		 * @return is existing option prefixed with <tt>-</tt>
		 */
		public boolean isOption(String inOption) {
			return getOptionIndex(inOption) > -1;
		}
		
		public String getArg(int inIndex) {
			return getArg(inIndex, true, null);
		}
		
		public String getArg(int inIndex, boolean isTrim, String inDefaultIfBlank) {

			inIndex = inIndex >= 0 ? inIndex : inIndex + args.length;

			String theArg = null;

			try {

				theArg = args[inIndex];
				theArg = !isTrim ? theArg : theArg.trim();
				theArg = theArg.isEmpty() ? null : theArg;

			} catch (Exception e) {
				// swallow
			}

			return theArg != null ? theArg : inDefaultIfBlank;
		}
		
		public boolean isOptionalArg(int inIndexPos) {
			return getOptionalArg(inIndexPos) != null;
		}
		
		public Option getOptionalArg(int inIndexPos) {
			
			final String theArg = Utils.getIndexedValueSafe(args, inIndexPos, null);
			if ( theArg == null ) { return null; }
			
			for (Option o : options) {
				if ( theArg.equals( "-"+o.option ) || theArg.equals( "--"+o.longoption ) ) {
					return o;
				}
			}
			
			return null;
		}

		public String getOptionParameter(String inOption) {
			
			final Option theOption = lookupOption( inOption );
			if ( !theOption.isOptionParameterized()) { return null; }
			
			final int theIndex = getOptionIndex( theOption );
			return theIndex == -1 ? null : getArg( theIndex+1 );
		}
		
		public Integer getOptionParameterAsInt(String inOption, Integer inDefault) {
			return Utils.parseInteger(getOptionParameter(inOption), inDefault);
		}
		
	}
}
