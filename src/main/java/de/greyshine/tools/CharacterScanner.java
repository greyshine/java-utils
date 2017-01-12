package de.greyshine.tools;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;

import de.greyshine.utils.deprecated.IFileTraverser;
import de.greyshine.utils.deprecated.Utils;

/**
 * Scans file(s) for a character which is identified by its number
 * 
 * args <file|dir> <character> <encoding>?
 * 
 * @author Dirk Schumacher
 */
public class CharacterScanner {

	static class W {
		int value = 0;
	}
	
	public static void main(String... args) throws IOException {
		
		if ( args.length < 1 ) { return; }
		final File file = new File( args[0] );
		final int targetChar = Integer.parseInt( args[1] );
		final Charset charset = args.length < 3 ? Charset.defaultCharset() : Charset.forName( args[2] );
		final boolean isSingleFile = !file.isDirectory();
		
		Utils.traversFiles( file, new IFileTraverser() {
			@Override
			public boolean handleFile(File inFile) throws Exception {
				
				scan( inFile, charset, targetChar, !isSingleFile );
				return true;
			}
		} );
		
	}
	
	private static void scan(File inFile, Charset charset, int targetChar, boolean printFileName ) throws IOException {
		
		final W line = new W();
		final W pos = new W();
		
		final String theFile = !printFileName ? "" : inFile.getAbsolutePath() +": ";
		
		Files.lines(inFile.toPath(), charset).forEach( (inLine)->{
			
			line.value++;
			pos.value = 0;
			
			inLine.chars().mapToObj( (i)->{return (char)i; } ).forEach( (c)->{
				
				if ( c == targetChar ) {
					System.out.println( theFile + line.value+":"+ pos.value );
				}
				
				pos.value++;
			} );
		} );
	}
	
}
