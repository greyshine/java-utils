package de.greyshine.tools;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.List;

import de.greyshine.utils.CommandLineParser;
import de.greyshine.utils.CommandLineParser.Args;
import de.greyshine.utils.Utils;
import de.greyshine.utils.Wrapper;

/**
 * Scans file(s) for a character which is identified by its number
 * 
 * args: <character> -e <encoding> <file>?
 * 
 * @author Dirk Schumacher
 */
public class CharacterScanner {

	final static CommandLineParser CLIP = new CommandLineParser()

			.option( "h" ).longoption( "help" ).optional().description( "print this message" ).done()//
			.option( "e" ).longoption( "encoding" ).optional().description( "encoding as specified by java.nio.charset.Charset namings" ).done()//
			.simpleArg("file").optional().description( "file or dir to scan" ).done()
			.generateUsageText( "charscan" );
	
	private static void displayHelp(String inErrorMessage) {
		
		if ( Utils.isNotBlank( inErrorMessage ) ) {
			System.err.println( inErrorMessage );
		}
		
		CLIP.printHelp();
	}
	
	public static void main(String... args) throws IOException {
		
		args = new String[]{"-h"};
		
		final Args theArgs = CLIP.parse(args);
		
		if ( theArgs.isOption( "h" ) ) {
			displayHelp(null);
			return;
		}
		
		final List<String> as = theArgs.getSimpleArgs();
		
		char theChar = (char)Utils.parseInteger( as.get(0) ).intValue();
		String theFile = as.size() > 1 ? as.get(1) : null; 
		String theCharset = theArgs.getOptionParameter( "e" ); 
		
		if ( Utils.isBlank( theFile ) && System.in.available() == 0 ) {
			theFile = ".";
		}
		
		if ( Utils.isNotBlank( theFile ) ) {
			
			scanStream(new File( theFile ), theCharset, theChar);
			
		} else {
			
			scanStream( System.in, theCharset, theChar );
		}
	}

	
	public static void scanStream(File inFile, String inCharset, char inTargetChar) throws IOException {
	
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
						scanStream( f, inCharset, inTargetChar );
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
		
		try ( InputStream is = new FileInputStream( inFile ) ) {
			
			scanStream( is , inCharset, inTargetChar, Utils.getCanonicalFile(inFile).getAbsolutePath());
		} 
	}
	
	public static void scanStream(InputStream inIs, String inCharset, char inTargetChar) throws IOException {
		scanStream(inIs, inCharset, inTargetChar, null);
	}
	
	public static void scanStream(InputStream inIs, String inCharset, char inTargetChar, String inLinePrefix) throws IOException {
		
		if ( inIs == null ) { return; }
		
		final Charset c = Utils.isBlank( inCharset ) ? Charset.defaultCharset() : Charset.forName( inCharset );
		final Wrapper<Integer> position = new Wrapper<>();
		final Wrapper<Integer> line = new Wrapper<>();
		final Wrapper<Integer> linePosition = new Wrapper<>();
		
		inLinePrefix = Utils.trimToEmpty(inLinePrefix);
		
		final String theLinePrefix  = inLinePrefix + (inLinePrefix.endsWith(":") ? " " : ": ");
		
		final BufferedReader r = new BufferedReader( new InputStreamReader( inIs , c) );
		while( r.ready() ) {
			
			final String theLine =r.readLine();
			line.value++;
			linePosition.value=-1;
			
			theLine.chars()
			.peek( (i)->{ position.value++; } )
			.mapToObj( (i)->{return (char)i; } )
			.filter( (i)->{ return i == inTargetChar; } )
			.forEach( (i)->{
				linePosition.value++;
				System.out.println( theLinePrefix + position +", "+ line.value +", "+ linePosition.value );
			} );
			
		}
		
		
		
		
	} 
	
}
