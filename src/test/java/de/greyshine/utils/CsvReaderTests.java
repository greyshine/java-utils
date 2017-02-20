package de.greyshine.utils;

import java.util.List;
import java.util.Map;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.beta.CsvReader;

public class CsvReaderTests {

	@Test
	public void testLIBREOFFICE_LINEPARSER() {
		
		String theLine = "a,b,c";
		List<String> theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		Assert.assertEquals( 3 , theCells.size() );
		Assert.assertEquals( "a" , theCells.get(0) );
		Assert.assertEquals( "b" , theCells.get(1) );
		Assert.assertEquals( "c" , theCells.get(2) );
		
		
		theLine = "a,\"b,b\",c";
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		
		Assert.assertEquals( 3 , theCells.size() );
		Assert.assertEquals( "a" , theCells.get(0) );
		Assert.assertEquals( "b,b" , theCells.get(1) );
		Assert.assertEquals( "c" , theCells.get(2) );

		theLine = "\"1,2\",b";
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		
		Assert.assertEquals( 2 , theCells.size() );
		Assert.assertEquals( "1,2" , theCells.get(0) );
		Assert.assertEquals( "b" , theCells.get(1) );

		theLine = "\",\",a";
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		
		Assert.assertEquals( 2 , theCells.size() );
		Assert.assertEquals( "," , theCells.get(0) );
		Assert.assertEquals( "a" , theCells.get(1) );
		
		theLine = "a,\",\"";
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		Assert.assertEquals( 2 , theCells.size() );
		Assert.assertEquals( "a" , theCells.get(0) );
		Assert.assertEquals( "," , theCells.get(1) );

		theLine = "\",\"\"\"";
		System.out.println( theLine );
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		Assert.assertEquals( 1 , theCells.size() );
		Assert.assertEquals( ",\"" , theCells.get(0) );

		theLine = "\"\"\",\"";
		System.out.println( theLine );
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		Assert.assertEquals( 1 , theCells.size() );
		Assert.assertEquals( "\"," , theCells.get(0) );
		
		theLine = "John Doe,\"\"\",\",911 Police Yrd,Muskogee 74403,,,,\"34,32\",19";
		System.out.println( theLine );
		theCells = CsvReader.LIBREOFFICE_LINEPARSER.parse( theLine );
		Assert.assertEquals( 9 , theCells.size() );
		Assert.assertEquals( "John Doe" , theCells.get(0) );
		Assert.assertEquals( "\"," , theCells.get(1) );
		Assert.assertEquals( "911 Police Yrd" , theCells.get(2) );
		Assert.assertEquals( "Muskogee 74403" , theCells.get(3) );
		Assert.assertEquals( "" , theCells.get(4) );
		Assert.assertEquals( "" , theCells.get(5) );
		Assert.assertEquals( "" , theCells.get(6) );
		Assert.assertEquals( "34,32" , theCells.get(7) );
		Assert.assertEquals( "19" , theCells.get(8) );
		
	}
	
	@Test
	public void test_Test1Csv() throws Exception {
		
		final StringBuilder sb = new StringBuilder();
		
		final CsvReader r = new CsvReader( CsvReader.LIBREOFFICE_LINEPARSER ).headings();
		
		r.read( Utils.getResource( "CsvReaderTests/test1.csv" ) ,  "UTF-8" , new CsvReader.Handler() {
			
			@Override
			public boolean begin(List<String> inHeadings) {
				sb.append( "#BEGIN:"+inHeadings );
				return true;
			}
			
			@Override
			public void done(int inRowCount) throws Exception {
				sb.append( "#DONE:"+inRowCount );
			}

			@Override
			public boolean row(int inRowIndex, Map<Integer, String> inValuesByIndex,
					Map<String, String> inValuesByName, Integer inHeadingDifference, String inLine) throws Exception {
				sb.append( "#ROW:"+inRowIndex+":"+ inValuesByIndex+":"+ inValuesByName+":"+inHeadingDifference+":"+inLine );
				return true;
			}

			@Override
			public boolean cell(int inRowIndex, String inColumnName, int inColumnIndex, String inValue) throws Exception {
				sb.append( "#CELL:"+inRowIndex+":"+ inColumnName+":"+ inColumnIndex+":"+inValue );
				return true;
			}
		});
		
		final String theExpected = "#BEGIN:[column1, column2]#ROW:1:{0=One, 1=1}:{column1=One, column2=1}:0:One,1#CELL:1:column1:0:One#CELL:1:column2:1:1#ROW:2:{0=Two, 1=2,2}:{column1=Two, column2=2,2}:0:Two,\"2,2\"#CELL:2:column1:0:Two#CELL:2:column2:1:2,2#DONE:2"; 
		final String theActual = sb.toString(); 
		
		System.out.println("Expected: "+ theExpected );
		System.out.println("Actual:   "+  theActual );
		
		Assert.assertEquals( theExpected , theActual );
	}
}
