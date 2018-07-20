package de.greyshine.utils;

import de.greyshine.utils.CommandLineParser.Args;

public class CommandLineParserExample {

	public static void main(String[] args) {
		
		CommandLineParser clp = new CommandLineParser()//
			.usageText( "invoke via Java main() of "+ CommandLineParserExample.class.getTypeName() )
			.headerText( "Exmaple for usage of "+ CommandLineParser.class.getTypeName() )//
			.footerText( "Some footer test" )
			.option("h", "help").description("displays this help text").done();
		
		Args theArgs = clp.parse(args);
		
		if ( theArgs.isEmpty() || theArgs.isOption("h") ) {
			
			System.out.println( "is: "+ theArgs.isOption("h") );
			clp.printHelp();
			return;
		}
		
	}
	
}
