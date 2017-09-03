package de.greyshine.utils.deprecated;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import de.greyshine.utils.ReflectionUtils;

public abstract class GsonUtils {
	
	private final static Log LOG = LogFactory.getLog(GsonUtils.class);

	private static final Map<String, Object> EMPTY_MAP_STRING_OBJECT = Collections.unmodifiableMap(new HashMap<String, Object>(0));
	private static final Collection<Object> EMPTY_COLLECTION = Collections.unmodifiableSet(new HashSet<Object>(0));
	private static final Map<Class<?>, Gson> GSONS = new HashMap<Class<?>, Gson>();

	private GsonUtils() {}
	
	public final static Gson getGson(Class<?> inClass) {

		if (inClass == null) {
			
			return null;
		} 

		Gson theGson = GSONS.get(inClass);

		if (theGson == null) {

			synchronized (GSONS) {

				theGson = GSONS.get(inClass);

				if (theGson == null) {

					final GsonBuilder theGb = new GsonBuilder()//
							.setDateFormat(Timer.DATE_ISO8601_ZULU)//
							.serializeNulls()//
							.excludeFieldsWithModifiers(Modifier.STATIC, Modifier.ABSTRACT, Modifier.TRANSIENT)//
							.disableHtmlEscaping();

					// check if class has a static TypeAdapter
					final List<Field> theTaFields = ReflectionUtils.getFields(inClass, TypeAdapter.class, true, true, null);

					if (theTaFields.size() == 1) {

						try {
							theGb.registerTypeAdapter(inClass, ReflectionUtils.getFieldValue(theTaFields.get(0), null));
						} catch (IllegalArgumentException | IllegalAccessException e) {

							throw new IllegalArgumentException("Class cannot derive TypeAdapter of " + inClass + ": " + e, e);
						}

					} else if (theTaFields.size() != 1) {

						LOG.warn("Ignoring " + inClass + " static final fields with " + TypeAdapter.class.getName() + "; not exact 1 matching TypeAdapter found; must be final static! found: " + theTaFields);
					}

					GSONS.put(inClass, theGson = theGb.create());
				}
			}
		}

		return theGson;
	}

	public static <T> T read(Class<T> inClass, File inIs) throws IOException {

		return inClass == null || inIs == null || !inIs.isFile() ? null : read(inClass, new FileInputStream(inIs), true);
	}

	public static <T> T read(Class<T> inClass, InputStream inIs, boolean inCloseInputStream) throws IOException {

		return read(inClass, inIs, Charset.defaultCharset(), inCloseInputStream);
	}

	public static <T> T read(Class<T> inClass, InputStream inIs, Charset inCharset, boolean inCloseInputStream) throws IOException {

		return inClass == null || inIs == null ? null : read(inClass, new InputStreamReader(inIs, inCharset == null ? Charset.defaultCharset() : inCharset), inCloseInputStream);
	}

	public static <T> T read(Class<T> inClass, Reader inIs, boolean inCloseInputStream) throws IOException {

		if (inIs == null || inClass == null) {

			return null;
		}

		try {

			return getGson(inClass).fromJson(inIs, inClass);

		} finally {

			if (inCloseInputStream) {

				Utils.close(inIs);
			}
		}
	}

	public static String toJson(Object inValue) {

		return inValue == null ? null : getGson(inValue.getClass()).toJson(inValue);
	}

	public static void write(String inFile, Charset inCharset, Object inValue) throws IOException {

		if (Utils.isNotBlank(inFile)) {

			write(new File(inFile), inCharset, inValue);
		}
	}

	private static void write(File file, Charset inCharset, Object inValue) throws IOException {

		if (file != null && file.exists()) {

			write(new FileOutputStream(file), inCharset, inValue, true);
		}
	}

	private static void write(OutputStream inOs, Charset inCharset, Object inValue, boolean inCloseStream) throws IOException {

		if (inOs != null) {

			write(new OutputStreamWriter(inOs, inCharset == null ? Charset.defaultCharset() : inCharset), inValue, inCloseStream);
		}
	}

	private static void write(Writer inWriter, Object inValue, boolean inCloseStream) throws IOException {

		try {

			if (inValue == null) {

				inWriter.write("null");
				inWriter.flush();

			} else {

				inWriter.write(getGson(inValue.getClass()).toJson(inValue));

			}

		} finally {

			if (inCloseStream) {

				Utils.close(inWriter);
			}
		}
	}

	public static void write(JsonWriter out, String inName, Object inObject) throws IOException {

		out.name(inName);
		write(out, inObject);
	}

	public static void write(JsonWriter out, Object inObject) throws IOException {

		if (inObject == null) {

			out.nullValue();
			
		} else {

			getGson(inObject.getClass()).toJson(inObject, inObject.getClass(), out);
		}
	}

	public static JsonElement parseJson(String inString) {

		return new com.google.gson.JsonParser().parse(inString == null ? "null" : inString);
	}

	public static JsonArray parseJsonArray(String inString) {

		final JsonElement theResult = parseJson(inString);

		return theResult.isJsonArray() ? theResult.getAsJsonArray() : null;
	}

	public static String getString(JsonElement inJsonElement, boolean inPrettyPrint) {

		if ( inPrettyPrint ) {
			
			return new GsonBuilder().setPrettyPrinting().create().toJson( inJsonElement );
			
		} else {
			
			return new GsonBuilder().create().toJson( inJsonElement );
			
		}
	}

	public static JsonElement toJsonElement(Object inObject) {
		
		if ( inObject == null ) { return JsonNull.INSTANCE; }
		else if ( inObject instanceof JsonElement ) { return (JsonElement) inObject; }
		else if ( inObject.getClass().isArray() ) {
			
			final JsonArray theArray = new JsonArray();
			
			for (int i = 0, l = Array.getLength( inObject ); i < l; i++) {

				theArray.add( toJsonElement( Array.get(inObject, i) ) );
			}
			
			return theArray;
		
		} else if ( inObject instanceof Boolean ) {
		
			return new JsonPrimitive( (boolean)inObject );
		
		} else if ( inObject instanceof Number ) {
			
			return new JsonPrimitive( (Number)inObject );
		
		} else if ( inObject instanceof IJsonWritable ) {
			
			try {
				
				return convertWritableToJsonElement( (IJsonWritable)inObject );
			
			} catch (IOException e) {
				
				throw Utils.toRuntimeException( e );
			}
		
		} else if ( inObject instanceof IJsonWritable ) {
			
			throw new UnsupportedOperationException("TODO implement");
		}
		
		return new JsonPrimitive( inObject.toString() );
	}

	public static JsonElement convertWritableToJsonElement(IJsonWritable inWritable) throws IOException {
		
		if ( inWritable == null ) { return JsonNull.INSTANCE; }
		
		final StringWriter theWriter = new StringWriter();
		
		inWritable.write( new JsonWriter( theWriter ) );
		
		return parseJson( theWriter.getBuffer().toString() );
	}

	public static IJsonWritable toWritable(final Charset inCharset, final boolean inPrettyPrint, final Object inObject) {
		
		return new AbstractJsonWritable() {
			
			{
				setCharset( inCharset );
				setPrettyPrint( inPrettyPrint );
			}
			
			@Override
			public void write(JsonWriter inWriter) throws IOException {
				
				if ( inObject == null ) { inWriter.nullValue(); }
				else if ( inObject instanceof IJsonWritable ) {
					
					((IJsonWritable)inObject).write( inWriter );					
				
				} else if ( inObject instanceof JsonElement ) {
				
					new JsonWritable( inCharset, inPrettyPrint, (JsonElement)inObject ).write( inWriter );
				
				} else if ( inObject instanceof Number ) {
					
					inWriter.value( (Number)inObject );
				
				} else if ( inObject.getClass().isArray() ) {
					
					inWriter.beginArray();
					
					for (int i = 0, l = Array.getLength( inObject ); i < l; i++) {
						
						toWritable(inCharset, inPrettyPrint, Array.get( inObject , i) ).write( inWriter );
					}
					
					inWriter.endArray();
				
				} else {
					GsonUtils.write(inWriter, inObject);
					//inWriter.value( inObject.toString() );
				}
			}
		};
	}
	
	/**
	 * Control for streaming an object w/ json
	 */
	public interface IJsonWritable {

		void write(JsonWriter inWriter) throws IOException;

		Charset getCharset();

		boolean isPrettyPrint();
	}
	
	public interface IJsonReadable {

		void read(JsonReader inReader) throws IOException;
	}

	public static abstract class AbstractJsonObject extends JsonElement {

		@Override
		public final boolean isJsonObject() {
			return true;
		}
		
		public abstract JsonObject getAsJsonObject();
	}
	
	public static abstract class AbstractJsonWritable implements IJsonWritable {

		private boolean isPrettyPrint = false;
		private Charset charset = de.greyshine.utils.Utils.CHARSET_UTF8;

		public AbstractJsonWritable() {

			this( null, false );
		}

		public AbstractJsonWritable(Charset inCharset) {

			this( inCharset, false );
		}
		
		public AbstractJsonWritable(Charset inCharset, boolean inPrettyPrint) {
			
			charset( inCharset );
			prettyPrint( inPrettyPrint );
		}

		@Override
		public Charset getCharset() {
			return charset;
		}

		@SuppressWarnings("unchecked")
		public <T> T charset(Charset inCharset) {
			setCharset(inCharset);
			return (T) this;
		}

		@SuppressWarnings("unchecked")
		public <T> T prettyPrint(boolean inPp) {
			setPrettyPrint(inPp);
			return (T) this;
		}

		public void setCharset(Charset inCharset) {
			charset = Utils.defaultIfNull(inCharset, de.greyshine.utils.Utils.CHARSET_UTF8);
		}

		@Override
		public boolean isPrettyPrint() {
			return isPrettyPrint;
		}

		public void setPrettyPrint(boolean inPp) {
			isPrettyPrint = inPp;
		}

		@Override
		public String toString() {

			return getClass().getName() + " [charset=" + charset + ", prettyPrint=" + isPrettyPrint + "]";
		}
	}

	public static class MapJsonWritable extends AbstractJsonWritable {

		private final Map<String, Object> map;

		public MapJsonWritable(Map<String, Object> inMap) {

			map = inMap != null ? inMap : EMPTY_MAP_STRING_OBJECT;
		}

		@Override
		public void write(JsonWriter inWriter) throws IOException {

			inWriter.beginObject();

			for (final String aKey : map.keySet()) {

				inWriter.name(aKey);
				GsonUtils.write(inWriter, map.get(aKey));

			}

			inWriter.endObject();
		}

	}

	public static class CollectionJsonWritable extends AbstractJsonWritable {

		private final Collection<?> collection;

		public CollectionJsonWritable(Collection<?> inCollection) {

			collection = inCollection != null ? inCollection : EMPTY_COLLECTION;
		}

		@Override
		public void write(JsonWriter inWriter) throws IOException {

			inWriter.beginArray();

			for (final Object o : collection) {

				GsonUtils.write(inWriter, o);
			}

			inWriter.endArray();
		}

	}
	
	public static class JsonWritable extends AbstractJsonWritable {

		public final JsonElement json;
		
		public JsonWritable(Charset inCharset, boolean inPrettyPrint, JsonElement inJson) {
			
			super( inCharset );
			setPrettyPrint( inPrettyPrint );
			
			json = inJson == null ? JsonNull.INSTANCE : inJson;
		}

		@Override
		public void write(JsonWriter inWriter) throws IOException {
		
			write( inWriter, json );
		}

		private void write(JsonWriter inWriter, JsonElement inJson) throws IOException {

			if ( inJson == null || inJson.isJsonNull() ) { inWriter.nullValue(); }
			else if ( inJson.isJsonArray() ) {
				
				final JsonArray theJa = inJson.getAsJsonArray();
				
				inWriter.beginArray();
				
				for (int i = 0, l = theJa.size(); i < l; i++) {
					
					write( inWriter, theJa.get( i ) );
				}
				
				inWriter.endArray();
			
			} else if ( inJson.isJsonObject() ) {

				final JsonObject theJo = inJson.getAsJsonObject();
				inWriter.beginObject();
				
				for (Entry<String,JsonElement> anEntry : theJo.entrySet()) {
					
					inWriter.name( anEntry.getKey() );
					write( inWriter, anEntry.getValue() );
				}
				
				inWriter.endObject();
			
			} else if ( inJson.isJsonPrimitive() ) {
				
				final JsonPrimitive theJp = inJson.getAsJsonPrimitive();
				
				if ( theJp.isBoolean() ) {
					
					inWriter.value( theJp.getAsBoolean() );
				
				} else if ( theJp.isNumber() ) {
					
					inWriter.value( theJp.getAsNumber() );
				
				} else {
					
					inWriter.value( theJp.getAsString() );
				}
				
			} else {
				
				throw new IllegalStateException("Must never happen. unknown JsonElement to write");
			}
		}
	}

	public static String prettyPrint(JsonElement inJson) {
		
		if ( inJson == null ) { return null; }
		
		return new GsonBuilder().setPrettyPrinting().create().toJson( inJson );
	}
}
