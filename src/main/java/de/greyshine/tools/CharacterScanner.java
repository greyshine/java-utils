package de.greyshine.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.function.Function;

import de.greyshine.utils.CommandLineParser;
import de.greyshine.utils.CommandLineParser.Args;
import de.greyshine.utils.Utils;
import de.greyshine.utils.Wrapper;

/**
 * Scans file(s) for a character which is identified by its number.
 */
public class CharacterScanner {

	final static CommandLineParser CLIP = new CommandLineParser()

			.option( "h", "help" ).optional().description( "print this message" ).done()//
			.option( "v" ).longoption("verbose").optional().description( "be verbose about what you are doing" ).done()//
			.option( "charset" ).optional().parameter("charset").description( "encoding as specified by java.nio.charset.Charset namings" ).done()//
			.option("charnum").optional().regex("[0-9]{0,3}").parameter( "number" ).description( "number of the char to scan for" ).done()
			.simpleArg("file").optional().description( "file or dir to scan" ).done()
			.generateUsageText( "charscan" );
	
	private static void displayHelp(String inErrorMessage) {
		
		if ( Utils.isNotBlank( inErrorMessage ) ) {
			System.err.println( inErrorMessage );
		}
		
		CLIP.printHelp();
	}
	
	public static void main(String... args) throws IOException {
		
		//args = new String[]{"-h"};
		
		final Args theArgs = CLIP.parse(args);
		
		if ( theArgs.isOption( "h" ) ) {
			displayHelp(null);
			return;
		}
		
		final boolean isVerbose = theArgs.isOption("v");
		
		Integer theChar = null;
		if ( theArgs.isOption( "charnum" ) ) {
			theChar = theArgs.getOptionParameterAsInt("charnum", null);
		}
		
		
		final Charset theCharset = Utils.defaultCharset(theArgs.getOptionParameter( "charset" ));
		textout( isVerbose , "charset="+ theCharset);
		
		final File theFile = Utils.defaultIfNull( theArgs.getSimpleArgAsFile(0) , Utils.BASEDIR);
		
		if ( theChar == null ) {
			
			textout(isVerbose, "scanning file(s) at "+ Utils.getCanonicalFile( theFile ));
			scanAllCharacters( theFile, theCharset );
			return;
		}
		
		textout(isVerbose, "scanning for character: "+ theChar.intValue() );
		scanStream( theFile , theCharset, (char)theChar.intValue(), isVerbose );
	}

	
	private static void textout(boolean isVerbose, String string) {
		if ( isVerbose ) {
			System.out.println( string );
		}
	}

	private static void scanAllCharacters(File inFile, Charset inCharset) throws IOException {
		
		final Function<File,Boolean> f = (aFile)->{
			
			if ( aFile == null || !aFile.isFile() ) { return true; }
			
			// System.out is intended!
			System.out.println( Utils.getCanonicalFile( aFile ).getAbsolutePath() );
			
			try ( BufferedReader r = new BufferedReader( new InputStreamReader( new FileInputStream( aFile ), inCharset ) ) ) {
			
				int linenum = -1;	
				final Wrapper<Integer> index = new Wrapper<>();
				
				while( r.ready() ) {
				
					final String theLine = r.readLine(); 
					final int theLineNum = ++linenum;
					
					index.value=0;
					
					theLine.chars().forEach( (i)->{
						
						// System.out is intended!
						System.out.println( theLineNum+" : "+ index.value++ +" : "+ i +" = "+ (char)i );
						
					} );
				}
			
			} catch(IOException e) {
				throw Utils.toRuntimeException(e);
			}
			
			return true;
		};
		
		try {
		
			Utils.traversFiles(inFile, f , true);
			
		} catch (Exception e) {
			throw Utils.toIOException(e, true);
		}
	}

	public static void scanStream(File inFile, Charset inCharset, char inTargetChar, final boolean inVerboseOutput) throws IOException {
	
		if ( Utils.isDir( inFile ) ) {

			// interesting building, oh my dear java; why do i have to do it that way?
			// btw: i am trying to throw the original IOException, ok having parallel execution is quiet difficult to solve the problem due to possibly have seveal exceptions at the same time
			class Rte extends RuntimeException {
				private static final long serialVersionUID = 10581708099403438L;
				Rte(IOException e) {
					super(e);
				}
			}
			
			try {
				Utils.listFiles(inFile, true).forEach( (f)->{
					
					try {
						scanStream( f, inCharset, inTargetChar, inVerboseOutput );
					} catch (IOException e) {
						throw new Rte(e);
					}
				});	
			} catch (Rte e) {
				throw (IOException)e.getCause();
			}
			
			return;
			
		} else if ( !Utils.isFile( inFile ) ) {
			return;
		}
		
		textout(inVerboseOutput, "scanning file: "+ inFile);
		
		try ( InputStream is = new FileInputStream( inFile ) ) {
			scanStream( is , inCharset, inTargetChar, Utils.getCanonicalFile(inFile).getAbsolutePath());
		} 
	}
	
	public static void scanStream(InputStream inIs, Charset inCharset, char inTargetChar) throws IOException {
		scanStream(inIs, inCharset, inTargetChar, null);
	}
	
	public static void scanStream(InputStream inIs, Charset inCharset, char inTargetChar, String inLinePrefix) throws IOException {
		
		if ( inIs == null ) { return; }
		
		inCharset = Utils.defaultIfNull( inCharset, Charset.defaultCharset());
		
		final Wrapper<Integer> position = new Wrapper<>(-1);
		final Wrapper<Integer> line = new Wrapper<>(-1);
		final Wrapper<Integer> linePosition = new Wrapper<>();
		
		inLinePrefix = Utils.trimToEmpty(inLinePrefix);
		
		final String theLinePrefix  = inLinePrefix + (inLinePrefix.endsWith(":") ? " " : ": ");
		
		try(BufferedReader r = new BufferedReader( new InputStreamReader( inIs , inCharset) )) {
		
			while( r.ready() ) {
				
				final String theLine = r.readLine();
				line.value++;
				linePosition.value=-1;
				
				theLine.chars()
				.peek( (i)->{
					position.value++;
					linePosition.value++;
				} )
				.mapToObj( (i)->{return (char)i; } )
				.filter( (i)->{ return i == inTargetChar; } )
				.forEach( (i)->{
					
					System.out.println( theLinePrefix + position.value +", "+ line.value +", "+ linePosition.value );
				} );
			}
		} 
	} 
	
}
