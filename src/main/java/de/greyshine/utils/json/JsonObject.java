package de.greyshine.utils.json;

import java.util.HashMap;
import java.util.Map;

public class JsonObject extends Json {
	
	private Map<String,Json> values = new HashMap<>();

	public JsonObject value(String inKey, String inValue) {
		values.put( dfin( inKey ), Json.of( inValue ) );
		return this;
	}
	
	private static String dfin(String inKey) {
		return inKey != null ? inKey : "";
	}

	public Json value(String inKey) {
		return values.get( inKey );
	}
	
	
}
