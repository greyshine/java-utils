package de.greyshine.utils;

import java.util.Collection;

import com.google.gson.JsonArray;
import com.google.gson.JsonNull;
import com.google.gson.JsonPrimitive;

public abstract class GsonUtils {

	private GsonUtils() {
	}
	
	public static JsonArray toArray(Collection<String> inStrings) {
		return toArray( inStrings, false );
	}
	
	public static JsonArray toArray(Collection<String> inStrings, final boolean inSkipNulls) {
		
		return inStrings == null ? new JsonArray() : inStrings.stream().collect( JsonArray::new, (inJa, inString) -> {
			
			if ( inString == null && inSkipNulls ) {
				return;
			}
			
			inJa.add( inString == null ? JsonNull.INSTANCE : new JsonPrimitive( inString ) );
			
		}, (inJa, inString) -> {} );
	}
	
}
