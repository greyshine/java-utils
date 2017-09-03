package de.greyshine.utils.beta;

import static de.greyshine.utils.Utils.isPrimeNumber;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.Utils;

/**
 * https://de.wikipedia.org/wiki/RSA-Kryptosystem
 */
public class PublicPrivateKeyManager {

	private byte[] privateKey;
	private byte[] publicKey;

	private long primePrivate, primePublic;
	private BigDecimal rsaModul;

	public PublicPrivateKeyManager() {
		
		BigDecimal thePublicPrime = new BigDecimal( System.currentTimeMillis() ).multiply( new BigDecimal(7) );
		BigDecimal thePrivatePrime = new BigDecimal( System.currentTimeMillis() ).multiply( new BigDecimal(13) );

		int c = 0;
		while( !isPrimeNumber( thePublicPrime ) ) {
			c++;
			thePublicPrime = BigDecimal.ONE.add( thePublicPrime );
		}

		thePrivatePrime = thePrivatePrime.multiply( new BigDecimal( c ) );
		while( !isPrimeNumber( thePrivatePrime ) && thePrivatePrime != thePublicPrime ) {
			thePrivatePrime = BigDecimal.ONE.add( thePrivatePrime );
		}
		
		Assert.assertTrue( isPrimeNumber( thePrivatePrime ) );
		Assert.assertTrue( isPrimeNumber( thePublicPrime ) );
		
		System.out.println();
		System.out.println( "pub prime: "+ thePublicPrime );
		System.out.println( "pvt prime: "+ thePrivatePrime );
		System.out.println( thePrivatePrime.subtract( thePublicPrime ) );

		rsaModul = thePublicPrime.multiply( thePrivatePrime );
		System.out.println( "rsa modul " + rsaModul );
		
		final BigDecimal euler2 = ( thePublicPrime.subtract( BigDecimal.ONE ) ).multiply( thePrivatePrime.subtract( BigDecimal.ONE ) );
		
		System.out.println( "euler: "+ euler2 );
	}

	public byte[] createPrivateKey() {

		return null;
	}

	public boolean isPublicKey(byte[] inPrivateKey, byte[] inPublicKey) {

		return false;
	}

	@Test
	public void test() {

		

		byte[] prk = createPrivateKey();
		byte[] pbk = createPublicKey(prk);

		Assert.assertTrue(isPublicKey(prk, pbk));
	}

	private byte[] createPublicKey(byte[] inPrivateKey) {

		return null;
	}

}
