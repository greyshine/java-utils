package de.greyshine.spielwiese;

import java.math.BigDecimal;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.Arrays;

import de.greyshine.utils.Utils;
import de.greyshine.utils.online.BitcoinKurs;

public class Spielwiese {

	public static void main(String[] args) throws Exception {

		DateTimeFormatter f = DateTimeFormatter.ofPattern("MMMM dd, yyyy");
		LocalDateTime ldt = LocalDateTime.from( f.parse("January 13, 2012") );
		System.out.println( ldt );
		System.out.println( ldt.toLocalDate() );
		System.out.println( ldt.toLocalTime() );
		
		LocalDate localDate = LocalDate.from(f.parse("January 13, 2012"));
		
		System.out.println( localDate );
		
	}

	static class Ppkp {

		byte[] privateKey;
		byte[] publicKey;

		public Ppkp() {

			try {
				KeyPairGenerator keyGen;
				keyGen = KeyPairGenerator.getInstance("RSA");
				KeyPair theKeyPair = keyGen.generateKeyPair();

				privateKey = theKeyPair.getPrivate().getEncoded();
				publicKey = theKeyPair.getPublic().getEncoded();

			} catch (NoSuchAlgorithmException e) {
				throw new RuntimeException(e);
			}
		}
		
		public String getPublicKeyBase64() {
			return Utils.toBase64( publicKey );
		}
		
		@Override
		public String toString() {
			return getPublicKeyBase64() + ", "+ Arrays.asList( privateKey );
		}
	}
	

}
