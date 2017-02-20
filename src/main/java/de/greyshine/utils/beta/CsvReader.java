package de.greyshine.utils.beta;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import de.greyshine.utils.Utils;

public class CsvReader {

	private List<String> headings;
	private ILineParser lineParser = LIBREOFFICE_LINEPARSER;
	
	public interface ILineParser {
		List<String> parse(String inLine );
	}

	public static void parseLibreOfficeWithUtf8WithHeadings( File inCsv, Handler inHandler ) throws Exception {
		new CsvReader( LIBREOFFICE_LINEPARSER ).headings().read( inCsv , "UTF-8", inHandler);
	}
	
	/**
	 * @param inLineParser the implementation of the line parser
	 */
	public CsvReader( ILineParser inLineParser ) {
		if ( inLineParser == null ) { throw new IllegalArgumentException(); }
		lineParser = inLineParser;
	}
	
	public CsvReader headings() {
		headings = new ArrayList<>();
		return this;
	}

	public CsvReader headings(String... inHeadings) {
		Utils.forEach(inHeadings, (h) -> {
			if (h != null) {
				headings = headings != null ? headings : new ArrayList<>();
				headings.add(h);
			}
		});
		return this;
	}

	public void read(File inFile, String inCharset, Handler inHandler) throws Exception {
		try ( FileInputStream fis = new FileInputStream( inFile ) ) {
			read( fis, inCharset, inHandler );
		} 
	}
	
	public void read(InputStream inIs, String inCharset, Handler inHandler) throws Exception {

		if (inHandler == null) {
			return;
		}

		final BufferedReader r = new BufferedReader(new InputStreamReader(inIs, Utils.defaultCharset(inCharset)));
		int rowIndex = 0;
		
		if (headings != null && headings.isEmpty() && r.ready() ) {

			lineParser.parse( r.readLine() ).forEach((c) -> {
				headings.add(Utils.defaultIfNull(c, ""));
			});
			rowIndex++;
		}
		
		boolean doContinue = inHandler.begin( headings == null ? null : new ArrayList<>( headings ) ); 
		
		while(  doContinue && r.ready() ) {
			
			final String theLine = r.readLine();
			final List<String> theCells = lineParser.parse( theLine );
			final Map<Integer,String> valuesByIndex = new LinkedHashMap<>( theCells.size() );
			final Map<String,String> valuesByName = new LinkedHashMap<>( theCells.size() );
			final Integer theHeadingsDifference = headings == null ? null : headings.size()-theCells.size(); 
			
			for( int i = 0, l = theCells.size(); i<l; i++ ) {
			
				valuesByIndex.put( i , theCells.get( i ));
				final String theHeading = headings==null||headings.size()<=i? String.valueOf( i ) :headings.get(i);  
				valuesByName.put( theHeading, theCells.get( i ));
			}
			
			doContinue = inHandler.row(rowIndex, valuesByIndex, valuesByName, theHeadingsDifference, theLine);

			if ( !doContinue ) { break; }
			
			for( int i = 0, l = theCells.size(); i<l; i++ ) {
				
				final String theHeading = headings==null||headings.size()<=i? String.valueOf( i ) :headings.get(i);
				doContinue = inHandler.cell(rowIndex, theHeading, i, theCells.get(i));
				if ( !doContinue ) { break; }
			}
			
			inHandler.rowDone(rowIndex, theCells);
			
			rowIndex++;
		}
		
		inHandler.done( rowIndex - (headings == null ? 0 : 1) );
	}

	public abstract static class Handler {

		public boolean begin(List<String> inHeadings) throws Exception {
			return true;
		}

		public void done(int inRowCount) throws Exception {}

		public boolean row(int inRowIndex, Map<Integer, String> inValuesByIndex,
				Map<String, String> inValuesByName, Integer inHeadingDifference, String inLine) throws Exception {
			return true;
		}

		public boolean cell(int inRowIndex, String inColumnName, int inColumnIndex, String inValue) throws Exception {
			return true;
		}
		
		public boolean rowDone( int inRowIndex, List<String> inCells ) throws Exception { return true; }
		
	}
	
	public static ILineParser LIBREOFFICE_LINEPARSER = new ILineParser() {
		
		@Override
		public List<String> parse(String inLine) {
		
			//System.out.println( "\nLINE: "+ inLine );
			
			final List<String> theCells = new ArrayList<>();

			final StringBuilder buffer = new StringBuilder();
			
			boolean isCurrentCellWithQuotes = false;
			boolean isFirstInCell = true;
			
			for( int i = 0, l = inLine.length(); i < l; i++ ) {
				
				boolean isLastCharacter = (i == l-1);
			
				char c = inLine.charAt(i);
				
				if ( buffer.length() == 0 && '"' == c ) {
					
					//start of cell
					isCurrentCellWithQuotes = true;
				}
				
				if ( !isCurrentCellWithQuotes ) {
					
					if ( ','==c || isLastCharacter ) {
					
						// eof simple cell
						if ( isLastCharacter ) {
							buffer.append( c );
						}
						theCells.add( buffer.toString() );
						buffer.setLength(0);	
						isCurrentCellWithQuotes = false;
					
					} else {
						
						buffer.append( c );
					}
					
					continue;
				
				} else {
					
					//System.out.println( "\tcell: "+ theCells.size() +": ["+ i +"]="+ c +" ("+ buffer +")" );
					
					if ( isLastCharacter ) {
						
						theCells.add( buffer.toString() );
						//System.out.println( "\tlast character; buffer=<"+ buffer +">" );
						continue;
					
					} else if ( isFirstInCell ) {
						
						// opening "
						isFirstInCell = false;
						//System.out.println( "\tbegin with \"; buffer=<"+ buffer +">" );
						continue;
					}
					
					final char cNext = inLine.charAt( i+1 );
					
					if ( c == '"' && cNext == ',' ) {

						//System.out.println( "\teof cell \": buffer=<"+ buffer +">");
						
						theCells.add( buffer.toString() );
						buffer.setLength(0);
						isCurrentCellWithQuotes = false;
						isFirstInCell = true;
						i++;
						continue;
					
					} else if ( c == '"' && cNext == '"'  ) {
					
						//System.out.println( "\tescaping \"; forward i++" );
						i++;
					}
					
					buffer.append( c );
					//System.out.println( "\tbuffer="+ buffer );
					
					continue;
				}
			}
			
			//System.out.println( "\tcells("+ theCells.size() +"): "+ theCells );
			
			return theCells;
		}
	};

}
