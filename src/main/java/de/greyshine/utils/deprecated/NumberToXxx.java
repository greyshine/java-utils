package de.greyshine.utils.deprecated;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class NumberToXxx {

	final static ThreadLocal<StringBuilder> TL_SB = new ThreadLocal<StringBuilder>() {

		@Override
		protected StringBuilder initialValue() {
			return new StringBuilder();
		}

		@Override
		public StringBuilder get() {
			final StringBuilder sb = super.get();
			sb.setLength(0);
			return sb;
		}
	};

	private final static char[] ALPHABET_atoz = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' };
	private final static char[] ALPHABET_AtoZ = { 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' };
	private final static char[] ALPHABET_atoz_AtoZ = { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' };
	private final static char[] ALPHABET_0to9_atoz = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' };
	private final static char[] ALPHABET_0to9_atoz_AtoZ = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' };

	final char[] chars;
	final BigDecimal mod;

	public NumberToXxx(char... inChars) {
		chars = inChars;
		mod = new BigDecimal(chars.length);

		final Set<Character> cs = new HashSet<Character>();

		for (int i = 0; i < inChars.length; i++) {
			if (cs.contains(inChars[i])) {
				throw new IllegalArgumentException("Each character must be definied once [index=" + i + ", char='" + inChars[i] + "']");
			} else {
				cs.add(inChars[i]);
			}
		}
	}

	public final static char[] getAlphabet_0to9_atoz() {
		return Arrays.copyOf(ALPHABET_0to9_atoz, ALPHABET_0to9_atoz.length);
	}

	public final static char[] getAlphabet_a2z() {
		return Arrays.copyOf(ALPHABET_atoz, ALPHABET_atoz.length);
	}

	public final static char[] getAlphabet_A2Z() {
		return Arrays.copyOf(ALPHABET_AtoZ, ALPHABET_AtoZ.length);
	}

	public final static char[] getAlphabet_atoz_AtoZ() {
		return Arrays.copyOf(ALPHABET_atoz_AtoZ, ALPHABET_atoz_AtoZ.length);
	}

	public final static char[] getAlphabet_0to9_atoz_AtoZ() {
		return Arrays.copyOf(ALPHABET_0to9_atoz_AtoZ, ALPHABET_0to9_atoz_AtoZ.length);
	}

	public String encode(Number inNumber) {

		BigDecimal n = new BigDecimal(inNumber.toString()).setScale(0, RoundingMode.FLOOR);

		if (n.compareTo(new BigDecimal(inNumber.toString())) != 0) {
			throw new IllegalArgumentException("Only non floating numbers are allowed.");
		} else if (BigDecimal.ZERO.compareTo(n) > 0) {
			throw new IllegalArgumentException("Only positive numbers allowed");
		} else if (BigDecimal.ZERO.compareTo(n) == 0) {
			return String.valueOf(chars[0]);
		}

		final boolean isNegativ = BigDecimal.ZERO.compareTo(n) > 0;
		n = !isNegativ ? n : n.negate();
		final StringBuilder sb = TL_SB.get();
		while (BigDecimal.ZERO.compareTo(n) != 0) {
			final BigDecimal r = n.remainder(mod);
			sb.append(chars[r.intValue()]);
			n = n.divide(mod, RoundingMode.FLOOR).setScale(0, RoundingMode.FLOOR);
		}

		return sb.reverse().toString();
	}

	public BigDecimal decode(String inNumber) {

		final char[] theChars = inNumber.toCharArray();
		BigDecimal high = BigDecimal.ONE;
		BigDecimal n = BigDecimal.ZERO;

		for (int i = theChars.length - 1; i > -1; i--) {
			n = n.add(high.multiply(getCharValue(theChars[i])));
			high = high.multiply(mod);
		}

		return n;
	}

	private BigDecimal getCharValue(char inChar) {
		for (int i = 0; i < chars.length; i++) {
			if (chars[i] == inChar) {
				return new BigDecimal(i);
			}
		}
		throw new IllegalArgumentException("Character is not in alphabet [char=" + inChar + ", alphabet=" + getAlphabetAsList(chars) + "]");
	}

	private List<Character> getAlphabetAsList(char[] inChars) {

		final List<Character> cs = new ArrayList<Character>(inChars.length);
		for (final Character character : inChars) {
			cs.add(character);
		}
		return cs;
	}

}