package de.greyshine.utils;

import java.util.List;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.CommandLineParser.Args;

public class CommandLineParserTests {

	@Test
	public void test() {
		
		String[] args = new String[] { "--value1", "cheated invisible value", "-v2", "'value 1'", "-v3" ,"rest1", "rest2", "\"rest 3\"" };
		
		final CommandLineParser theClp = new CommandLineParser();
		theClp.option( "v1" ).longoption( "value1" );
		theClp.option( "v2" ).parameter( "v2argname").description( "worx somehow" );
		theClp.option( "v3" ).optional().description( "worx somehow too\nmultilined\nnext line" );
		theClp.simpleArg("rest1").description( "a must have parameter" );
		theClp.simpleArg("rest2").multi().description( "can have parameters, but at least 1" );
		
		
		theClp.headerText( "Some text before usage.\nSee here how it works:" );
		theClp.footerText( "\nHave fun using it." );
		theClp.generateUsageText( "commandy" );

		theClp.printHelp();

		final Args theArgs = theClp.parse( args );
		
		Assert.assertTrue( theArgs.isOption( "v1" ) );
		Assert.assertEquals( 0, theArgs.getOptionIndex( "v1" ) );
		Assert.assertEquals( 0, theArgs.getOptionIndex( "value1" ) );
		Assert.assertTrue( theArgs.isOption( "v2" ) );
		Assert.assertTrue( theArgs.isOption( "v3" ) );
		Assert.assertFalse( theArgs.isOption( "cheated invisible value" ) );
		Assert.assertFalse( theArgs.isOption( "value 1" ) );
		Assert.assertFalse( theArgs.isOption( "rest1" ) );
		Assert.assertFalse( theArgs.isOption( "rest2" ) );
		Assert.assertFalse( theArgs.isOption( "rest 3" ) );
		

		Assert.assertEquals( 0, theArgs.getOptionIndex( "v1" ) );
		Assert.assertEquals( 0, theArgs.getOptionIndex( "v1" ) );
		Assert.assertEquals( 2, theArgs.getOptionIndex( "v2" ) );
		Assert.assertEquals( 4, theArgs.getOptionIndex( "v3" ) );
		Assert.assertEquals("value 1", theArgs.getOptionParameter( "v2" ));
		
		List<String> theRestArgs = theArgs.getSimpleArgs();
		Assert.assertEquals( theRestArgs.toString(), 3 , theRestArgs.size());
		Assert.assertEquals( "rest1" , theRestArgs.get(0));
		Assert.assertEquals( "rest2" , theRestArgs.get(1));
		Assert.assertEquals( "rest 3" , theRestArgs.get(2));
	}
	
}
