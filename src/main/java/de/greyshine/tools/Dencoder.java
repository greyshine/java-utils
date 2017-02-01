package de.greyshine.tools;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Iterator;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import de.greyshine.utils.Utils;

@SuppressWarnings({ "static-access", "deprecation" })
public class Dencoder {
	
	private static final CommandLineParser CLI_PARSER = new DefaultParser();
	private static final Options CLI_OPTIONS = new Options();
	
	static {
		
		CLI_OPTIONS.addOption( OptionBuilder.withLongOpt("help").withDescription( "print this message" ).create("h") );
		CLI_OPTIONS.addOption( OptionBuilder.withLongOpt( "quiet").withDescription( "no error output" ).create("q") );
		CLI_OPTIONS.addOption( OptionBuilder.withArgName( "file" ).withDescription( "file to read" ).hasOptionalArg().create("f") );
		CLI_OPTIONS.addOption( OptionBuilder.withArgName( "file" ).withDescription( "file to write" ).hasOptionalArg().create("o") );
		CLI_OPTIONS.addOption( OptionBuilder.withArgName( "password" ).withDescription( "password" ).hasOptionalArg().create("p") );
		CLI_OPTIONS.addOption( OptionBuilder.withLongOpt( "encode").withDescription( "encode direction, only encode or decode allowed" ).create("e") );
		CLI_OPTIONS.addOption( OptionBuilder.withLongOpt( "decode").withDescription( "decode direction, only encode or decode allowed" ).create("d") );
	}
	
	private synchronized static CommandLine parseCli( String[] args ) {
		
		if ( args.length == 0 ) { return null; }
		
		try {
		
			return CLI_PARSER.parse( CLI_OPTIONS, args);  
		
		} catch (ParseException e) {
			System.out.println( e.getMessage() );
			printHelp();
			return null;
		}
	}
	
	private static void printHelp(  ) {
		new HelpFormatter().printHelp("dencode", CLI_OPTIONS );		
	}
	
	public static void main(String[] args) throws Exception {
		
		final CommandLine cli = parseCli(args);
		if ( cli == null ) { return; }
		
		boolean isQuiet = cli.hasOption( 'q' ) || cli.hasOption( "quiet" ); 
		
		if ( cli.hasOption( "help" ) || cli.hasOption( "h" ) ) {
			if (!isQuiet) { printHelp(); }
			return ;
		}
		
		if ( (cli.hasOption( 'e' ) && cli.hasOption( 'd' )) || (!cli.hasOption( 'e' ) && !cli.hasOption( 'd' )) ) {
			if (!isQuiet) { System.out.println("only -d or -e allowed."); }
			if (!isQuiet) { printHelp(); }
			return;
		}
		
		if ( cli.hasOption( 'e' ) ) {

			encode( cli );
			return;
			
		} else if ( cli.hasOption( 'd' ) ) {
			
			decode( cli );
			return;
			
		}
	}
	
	private static Iterator<Integer> createPasswordIterator(String password) {
		
		return Utils.isBlank( password ) ? null : new Iterator<Integer>() {
		
			int counts = password.trim().length();
			
			final byte[] bytes = password.trim().getBytes( Utils.CHARSET_UTF8 );
			int idx = 0;
			
			@Override
			public boolean hasNext() {
				return true;
			}
			@Override
			public Integer next() {
				
				if ( idx >= bytes.length ) { idx=0; }	
				
				int r = ++counts;
				r += (int) bytes[idx++];
				r *= (r < 0 ? -1 : 1);
				r = r % 255;

				return r;
			}
		};
	}

	public static void encode(CommandLine cli) throws Exception {
		
		boolean isInputFile = cli.hasOption( 'f' );
		boolean isOutputFile = cli.hasOption( 'o' );
		final InputStream is = !isInputFile ? System.in : new FileInputStream( cli.getOptionValue('f') );
		final OutputStream os = !isOutputFile ? System.out : new FileOutputStream(Utils.mkParentDirs( cli.getOptionValue('o') ));
		
		encode( is, os, cli.getOptionValue('p') );
		
		if ( isInputFile ) { Utils.close(is); }
		if ( isOutputFile ) { Utils.close(os); }
	}

	private static void encode(InputStream is, OutputStream os, String password) throws IOException {
		
		Iterator<Integer> thePwdIter = createPasswordIterator( password );
		
		while( is.available() > 0 ) {
			
			int r = is.read();
			
			r = thePwdIter == null ? r : r ^ thePwdIter.next();
			
			String theHex = Integer.toString( r,  Character.MAX_RADIX ); 
			
			if ( theHex.length() < 2 ) {
				theHex = "0"+theHex;
			}
			
			os.write( theHex.getBytes() );
		}
		
		os.flush();
	}

	private static void decode(CommandLine cli) throws IOException {
		
		boolean isInputFile = cli.hasOption( 'f' );
		boolean isOutputFile = cli.hasOption( 'o' );
		final InputStream is = !isInputFile ? System.in : new FileInputStream( cli.getOptionValue('f') );
		final OutputStream os = !isOutputFile ? System.out : new FileOutputStream(Utils.mkParentDirs( cli.getOptionValue('o') ));
		
		decode( is, os, cli.getOptionValue('p') );
		
		if ( isInputFile ) { Utils.close(is); }
		if ( isOutputFile ) { Utils.close(os); }
	}
	
	public static void decode(InputStream is, OutputStream os, String password) throws IOException {
		
		Iterator<Integer> thePwdIter = createPasswordIterator( password );
		
		while( is.available() > 0 ) {
			
			int r1 = is.read(); 
			int r2 = is.read(); 
			
			int r = Integer.parseInt((char)r1 +""+ (char)r2, Character.MAX_RADIX);
			
			r = thePwdIter == null ? r : r ^ thePwdIter.next();
			
			os.write( (char)r );
		}
		
		os.flush();
	}

}
