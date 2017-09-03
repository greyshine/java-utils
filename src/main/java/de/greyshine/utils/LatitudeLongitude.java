package de.greyshine.utils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.regex.Pattern;

/**
 * Mongo: lon,lat
 * google: lat,lon
 * @author greyshine
 *
 */
public class LatitudeLongitude {

	public static final Pattern REGEX = Pattern.compile("\\-?[0-9]+(\\.[0-9]+)?\\,\\-?[0-9]+(\\.[0-9]+)?");

	private String lat, lon;

	public LatitudeLongitude() {

		lat = lon = "0.000000";
	}

	public LatitudeLongitude(String lat, String lon) {

		setLat(lat);
		setLon(lon);
	}

	public LatitudeLongitude(double lat, double lon) {

		setLat(lat);
		setLon(lon);
	}

	public LatitudeLongitude(String inLatCommaLng) {

		final String[] theValues = Utils.trimToEmpty(inLatCommaLng).split(",");

		if (theValues == null || theValues.length != 2 || !Utils.isParseableDouble(theValues[0].trim()) || !Utils.isParseableDouble(theValues[1].trim())) {

			throw new IllegalArgumentException("bad latitude,longitude: " + inLatCommaLng);
		}

		setLat(theValues[0].trim());
		setLon(theValues[1].trim());
	}

	public void setLat(String lat) {

		final BigDecimal theLat = Utils.parseBigDecimal(lat);

		if (theLat == null) {
			throw new IllegalArgumentException("bad latitude: " + lat);
		}

		setLat(theLat);
	}

	public void setLat(double inLatitude) {

		setLat(new BigDecimal(inLatitude));
	}

	public void setLat(BigDecimal inBd) {

		lat = normalizeLatitude( inBd ).toPlainString();
	}

	public void setLon(String lon) {

		final BigDecimal theLon = Utils.parseBigDecimal(lon);

		if (theLon == null) {
			throw new IllegalArgumentException("bad longitude: " + lat);
		}

		setLon(theLon);
	}
	public void setLon(double inLongitude) {

		setLon(new BigDecimal(inLongitude));
	}

	public void setLon(BigDecimal inBd) {

		lon = normalizeLongitude(inBd).toPlainString();
	}
	
	public String getLat() {
		return lat;
	}

	public String getLon() {
		return lon;
	}

	public static LatitudeLongitude parseLatLon(String inValue) {

		if (Utils.isMatch(inValue, REGEX)) {
			throw new IllegalArgumentException("Cannot parse: " + inValue);
		}

		try {
			final int cidx = inValue.indexOf(',');
			return new LatitudeLongitude(inValue.substring(0, cidx).trim(), inValue.substring(cidx + 1).trim());
		} catch (final Exception e) {
			throw new IllegalArgumentException("Cannot parse: " + inValue);
		}
	}

	public static LatitudeLongitude parseLonLat(String inValue) {

		if (Utils.isMatch(inValue, REGEX)) {
			throw new IllegalArgumentException("Cannot parse: " + inValue);
		}

		try {
			final int cidx = inValue.indexOf(',');
			return new LatitudeLongitude(inValue.substring(cidx + 1).trim(), inValue.substring(0, cidx).trim());
		} catch (final Exception e) {
			throw new IllegalArgumentException("Cannot parse: " + inValue);
		}
	}

	public double[] toLatLonDoubles() {

		return new double[] { getLatitude(), getLongitude() };
	}

	public double[] toLonLatDoubles() {

		return new double[] { getLongitude(), getLatitude() };
	}
	
	public static double normalizeLatitude(double inLat) {
		
		return normalizeLatitude( new BigDecimal( inLat ) ).doubleValue();
	}
	
	public static double normalizeLongitude(double inLat) {
		
		return normalizeLongitude( new BigDecimal( inLat ) ).doubleValue();
	}
	
	public static BigDecimal normalizeLatitude(BigDecimal inLat) {
		
		if (inLat == null) {
			throw new IllegalArgumentException();
		}
		
		while (Utils.isLess(Utils.BD_m90, inLat)) {

			inLat = inLat.add(Utils.BD_180);
		}

		while (Utils.isLarger(Utils.BD_90, inLat)) {

			inLat = inLat.subtract(Utils.BD_180);
		}

		return inLat.setScale(6, RoundingMode.HALF_UP);
	}
	
	public static BigDecimal normalizeLongitude(BigDecimal inLon) {
		
		if (inLon == null) {
			throw new IllegalArgumentException();
		}
		
		while (Utils.isLess(Utils.BD_m180, inLon)) {

			inLon = inLon.add(Utils.BD_360);
		}

		while (Utils.isLarger(Utils.BD_180, inLon)) {

			inLon = inLon.subtract(Utils.BD_360);
		}

		return inLon.setScale(6, RoundingMode.HALF_UP);
	}

	public double getLongitude() {
		return Double.parseDouble(lon);
	}

	public double getLatitude() {
		return Double.parseDouble(lat);
	}

	public String toLatLonString() {
		return lat + "," + lon;
	}

	public String toLonLatString() {
		return lon + "," + lat;
	}

	@Override
	public String toString() {
		return "LatitudeLongitude [lat=" + lat + ", lon=" + lon + "]";
	}
}
