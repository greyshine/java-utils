package de.greyshine.utils.json;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.Charset;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;


public abstract class Json {
	
	static final Charset UTF8 = Charset.forName( "utf-8" );
	
	private static final Gson DEFAULT_JSON_2_STRING = new GsonBuilder().create();
	private static final Gson DEFAULT_JSON_2_STRING_PRETTYPRINT = new GsonBuilder().setPrettyPrinting().create();
	
	private Json parent;
	
	Json() {
		
	}
	
	public final String getAsString(boolean inPrettyPrint) {
		return "";
	}
	
	public final InputStream getAsInputStream(boolean inPrettyPrint) {
		return new ByteArrayInputStream( getAsString(inPrettyPrint).getBytes( UTF8 ) );
	}
	
	public static JsonObject createObject() {
		return new JsonObject();
	}
	
	public static JsonBoolean createBoolean(boolean inBoolean) {
		return new JsonBoolean(inBoolean);
	}

	public static JsonString createString(String inText) {
		return new JsonString(inText);
	}

	public static Json of(String inValue) {
		return JsonString.of(inValue);
	}
	public static Json of(Boolean inValue) {
		return JsonBoolean.of(inValue);
	}

}
