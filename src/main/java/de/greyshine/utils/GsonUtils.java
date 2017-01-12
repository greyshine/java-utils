package de.greyshine.utils;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.google.gson.JsonArray;
import com.google.gson.JsonNull;
import com.google.gson.JsonPrimitive;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public abstract class GsonUtils {

	public static final TypeAdapter<LocalDateTime> TYPEADAPTER_LOCALDATETIME = new TypeAdapter<LocalDateTime>() {
		@Override
		public void write(JsonWriter out, LocalDateTime value) throws IOException {
			if ( value == null ) {
				out.nullValue();
			} else {
				out.value( value.format( DateTimeFormatter.ISO_DATE_TIME ) );
			}
		}
		@Override
		public LocalDateTime read(JsonReader in) throws IOException {
			switch (in.peek()) {
			case NULL:
				in.nextNull();
				return null;
			default:
				String theValue = in.nextString();
				return theValue==null||theValue.trim().isEmpty() ? null : LocalDateTime.parse( theValue.trim(), DateTimeFormatter.ISO_DATE_TIME);
			}
		}
	};

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
