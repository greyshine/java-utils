package de.greyshine.utils;

import java.io.IOException;
import java.lang.reflect.Type;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

public abstract class GsonUtils {
	
	/**
	 * Public exposed so the application may change global behavior
	 */
	public static final GsonBuilder GSONBUILDER = new GsonBuilder();

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
	
	public static JsonArray toJsonArray(Collection<String> inStrings) {
		return toJsonArray( inStrings, false );
	}
	
	public static void registerTypeAdapter(Type inType, TypeAdapter<?> inTypeAdaper) {
		
		if ( inType == null || inTypeAdaper == null ) {
			return; 
		}
		
		GSONBUILDER.registerTypeAdapter( inType , inTypeAdaper);
	}
	
	public static JsonArray toJsonArray(Collection<String> inStrings, final boolean inSkipNulls) {
		
		return inStrings == null ? new JsonArray() : inStrings.stream().collect( JsonArray::new, (inJa, inString) -> {
			
			if ( inString == null && inSkipNulls ) {
				return;
			}
			
			inJa.add( inString == null ? JsonNull.INSTANCE : new JsonPrimitive( inString ) );
			
		}, (inJa, inString) -> {} );
	}

	public static JsonArray toArray(Collection<? extends JsonElement> inValues) {
		return toArray(inValues,false);
	}
	
	public static JsonArray toArray(Collection<? extends JsonElement> inValues, final boolean inSkipNulls) {
		
		final JsonArray ja = new JsonArray();
		
		if ( inValues == null ) { return ja; }
		
		inValues.stream().forEach( (i) -> { 
			
			if ( i==null && inSkipNulls ) { return; }
			else if ( i==null ) { ja.add( JsonNull.INSTANCE ); }
			else {
				ja.add( i );
			}
		} );
		
		return ja;
	}

	public static String getAsJsonStringSafe(JsonObject inJson, String inMember, String inDefault) {
		
		if ( inJson == null ) { return inDefault; }
		
		final JsonElement jp = inJson.get( inMember );
		
		final String s = jp == null || !jp.isJsonPrimitive() ? null : jp.getAsString(); 
		
		return s == null ? inDefault : s;
	}
	
	public static JsonElement convert( Object inJavaObject ) {
		
		if ( inJavaObject == null ) { return JsonNull.INSTANCE; }
		
		final Gson gson = GSONBUILDER.create();
		
		final String theJson = gson.toJson( inJavaObject );
		
		return new JsonParser().parse( theJson );
	}
	
	public static JsonArray convert( Collection<?> c) {
		
		final JsonArray ja = new JsonArray();
		
		if ( c==null ) { return ja; }
		
		c.forEach( e->{ ja.add( convert( e ) ); } );
		
		return ja;
	}
}
