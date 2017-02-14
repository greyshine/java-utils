package de.greyshine.utils.json;

public class JsonBoolean extends Json {
	
	private boolean value;

	public JsonBoolean(boolean inValue) {
		value = inValue;
	}
	
	public boolean getValue() {
		return value;
	}

	public JsonBoolean value(boolean inValue) {
		value = inValue;
		return this;
	}
	
	public static JsonBoolean of(Boolean inValue) {
		return inValue == null ? null : new JsonBoolean( inValue );
	}

	
	
}
