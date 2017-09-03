package de.greyshine.utils.beta;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.tika.Tika;
import org.apache.tika.exception.TikaException;
import org.apache.tika.metadata.Metadata;
import org.apache.tika.parser.AutoDetectParser;
import org.apache.tika.parser.ParseContext;
import org.apache.tika.parser.pdf.PDFParser;
import org.apache.tika.sax.BodyContentHandler;
import org.xml.sax.SAXException;

public final class TikaUtils {

	private TikaUtils() {
	}

	public static String pdfToString(File inFile) throws IOException, SAXException, TikaException {

		if (inFile == null || inFile.isDirectory()) {
			return null;
		}

		InputStream input = new FileInputStream(inFile);
		BodyContentHandler handler = new BodyContentHandler(Integer.MAX_VALUE);
		Metadata metadata = new Metadata();
		
		new PDFParser().parse(input, handler, metadata, new ParseContext());
		
		return handler.toString();
	}
	
	public static void parsePdf( File inPdfFile, BiConsumer<Integer,String> inLineConsumer ) throws IOException, SAXException, TikaException {
		
		final String theText = pdfToString(inPdfFile);
		
		if ( theText == null || inLineConsumer == null ) { return; }
		
		class LineIndex {
			int index = 0;
			int next() {
				return index++;
			}
		}
		
		final LineIndex theLineIndex = new LineIndex();
		
		Arrays.stream( theText.split( "\n" , -1) ).forEach( (inLine)->{ inLineConsumer.accept(theLineIndex.next(), inLine); } );
		
	}
	
	
	

}
