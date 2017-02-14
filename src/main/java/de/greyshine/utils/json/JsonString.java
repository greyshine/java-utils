package de.greyshine.utils.json;

public class JsonString extends Json {

	private String value;

	public JsonString(String inValue) {
		value( inValue );
	}
	
	public JsonString value(String inValue) {
		if ( inValue == null ) {
			throw new IllegalArgumentException("string must not be null");
		}
		value = inValue;
		return this;
	}
	
	public String value() {
		return value;
	}

	public static Json of(String inValue) {
		return inValue == null ? null : new JsonString( inValue );
	}
}
