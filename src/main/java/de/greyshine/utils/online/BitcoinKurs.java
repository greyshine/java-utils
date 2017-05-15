package de.greyshine.utils.online;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.greyshine.utils.Utils;

public class BitcoinKurs {

	private final static Log LOG = LogFactory.getLog( BitcoinKurs.class );
	
	public LocalDateTime date;
	public BigDecimal value;
	
	private BitcoinKurs() {
		
	}
	
	@Override
	public String toString() {
		return getClass().getTypeName()+" [date="+ Utils.formatDate("yyyy-MM-dd HH:mm", date) +", value="+ (value==null?"":value.toPlainString()) +"]";
	}
	
	public static BitcoinKurs fetchAtBitcoinDe() throws Exception {

		ByteArrayOutputStream theOut = new ByteArrayOutputStream();
		ByteArrayOutputStream theErr = new ByteArrayOutputStream();
		
		Utils.runConsole(new File("."), theOut, theErr, "curl", "-q", "https://www.bitcoin.de");
		
		if ( theOut.toByteArray().length == 0 ) {
			theOut = null;
			throw new IOException( theErr.toString() );
		}
		
		final String s = new String( theOut.toByteArray() );
		theOut = null;
		
		List<String> theLines = Utils.grep( s, "Aktueller", "ticker_price", "Bitcoin" );
		String thePrice = Utils.unwrap( theLines.isEmpty() ? null : theLines.remove(0), "<strong id=\"ticker_price\">", " €");
		thePrice = thePrice == null ? null : thePrice.replaceAll("\\.","").replaceAll( "," , ".");
		
		theLines = Utils.grep( s, "Stand ", "(", ")" );
		theLines = Utils.egrep( Utils.concatLines( theLines, "\n" ), "[0123][0-9]\\.[012][0-9]\\.[0-9]{2}" );
		
		String theDate = theLines.get( 0 ).replaceAll( "[Stand()]" , "").trim();
		
		final BitcoinKurs bk = new BitcoinKurs();
		
		bk.value = new BigDecimal( thePrice );
		bk.date = Utils.parseDate( "dd.MM.yy HH:mm", theDate );
		
		return bk;
	}
	
}
