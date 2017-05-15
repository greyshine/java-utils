package de.greyshine.spielwiese;

import de.greyshine.utils.online.BitcoinKurs;

public class Spielwiese {

	public static void main(String[] args) throws Exception {
		
		System.out.println( BitcoinKurs.fetchAtBitcoinDe() );
	}
	
	
	
}
