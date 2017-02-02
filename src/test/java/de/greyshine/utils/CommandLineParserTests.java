package de.greyshine.utils;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.CommandLineParser.Args;

public class CommandLineParserTests {

	@Test
	public void test() {
		
		String[] args = new String[] { "--value1", "-v2", "value1", "-v3" ,"rest1", "rest2" };
		
		final CommandLineParser theClp = new CommandLineParser();
		theClp.option( "v1" ).longoption( "value1" );
		theClp.option( "v2" ).parameter( "v2argname").description( "worx somehow" );
		theClp.option( "v3" ).parameter( "v3argname" ).description( "worx somehow too\nmultilined\nnext line" );
		theClp.simpleArg("rest1").description( "a must have parameter" );
		theClp.simpleArg("rest2").multi().description( "can have parameters, but at least 1" );
		
		
		theClp.headerText( "Some text before usage.\nSee here how it works:" );
		theClp.footerText( "\nHave fun using it." );
		theClp.generateUsageText( "commandy" );

		theClp.printHelp();

		final Args theArgs = theClp.parse( args );

		Assert.assertEquals( 0, theArgs.getOptionIndex( "v1" ) );
		Assert.assertEquals( 1, theArgs.getOptionIndex( "v2" ) );
		Assert.assertEquals( 3, theArgs.getOptionIndex( "v3" ) );
		Assert.assertEquals("value1", theArgs.getOptionParameter( "v2" ));
		
		List<String> theRestArgs = theArgs.getSimpleArgs();
		Assert.assertEquals( theRestArgs.toString(), 2 , theRestArgs.size());
		Assert.assertEquals( "rest1" , theRestArgs.get(0));
		Assert.assertEquals( "rest2" , theRestArgs.get(1));
	}
	
}
