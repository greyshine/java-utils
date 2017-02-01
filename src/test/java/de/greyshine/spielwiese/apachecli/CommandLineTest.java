package de.greyshine.spielwiese.apachecli;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.junit.Test;

public class CommandLineTest {

	private static final CommandLineParser CLI_PARSER = new DefaultParser();
	private static final Options CLI_OPTIONS = new Options();
	
	
	@Test
	public void run() throws ParseException {
		
		CLI_OPTIONS.addOption( OptionBuilder.withDescription( "some option" ).create("f") );
		
		String[] args = new String[]{ "-f", "someoptionvalue", "anyvalue1", "anyvalue2" };
		
		final CommandLine cli = CLI_PARSER.parse(CLI_OPTIONS, args, false);
		System.out.println( cli.getArgList() );
		
		
		
		
		
		
		
	}
	
}
