package de.greyshine.utils.beta;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.function.Consumer;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;

import de.greyshine.utils.Utils;

public class JsonPersister {
	
	private static final Charset UTF8 = Charset.forName( "UTF-8" );
	
	private Gson gson;
	
	/**
	 * Does not work so far...<br/>
	 * What to todo when reading an empty object?
	 */
	@Deprecated
	public static TypeAdapter<Object> TYPEADAPTER_OBJECT = new TypeAdapter<Object>() {

		@Override
		public void write(JsonWriter out, Object value) throws IOException {
			if ( value == null ) {
				out.nullValue();
			} else {
				out.beginObject().endObject();
			}
		}

		@Override
		public Object read(JsonReader in) throws IOException {
			
			switch ( in.peek() ) {
			case NULL:
				return null;
			case END_OBJECT:
				return new Object();
			default:
				throw new UnsupportedOperationException("not yet implemented");
			}
		}
	};
	
	public static TypeAdapter<LocalDate> createLocalDateAdapter(String inPattern) {
		
		return new TypeAdapter<LocalDate>() {

			final DateTimeFormatter dtf = new DateTimeFormatterBuilder().appendPattern( inPattern ).toFormatter();
			
			@Override
			public void write(JsonWriter out, LocalDate value) throws IOException {
				
				if ( value == null ) {
				
					out.nullValue();
				
				} else {
		
					out.value( dtf.format( value ) );
				}
			}

			@Override
			public LocalDate read(JsonReader in) throws IOException {
				
				switch ( in.peek() ) {
				
				case NULL:
					
					return null;
					
				case STRING:
					
					return LocalDate.from( dtf.parse( in.nextString() ) );

				default:
					throw new IOException( "bad token: "+ in.peek() );
				}
			}
		};
	};

	public static TypeAdapter<LocalDateTime> createLocalDateTimeAdapter(String inPattern) {
		
		return new TypeAdapter<LocalDateTime>() {
			
			final DateTimeFormatter dtf = new DateTimeFormatterBuilder().appendPattern( inPattern ).toFormatter();
			
			@Override
			public void write(JsonWriter out, LocalDateTime value) throws IOException {
				
				if ( value == null ) {
					
					out.nullValue();
					
				} else {
					
					out.value( dtf.format( value ) );
				}
			}
			
			@Override
			public LocalDateTime read(JsonReader in) throws IOException {
				
				switch ( in.peek() ) {
				
				case NULL:
					
					return null;
					
				case STRING:
					
					return LocalDateTime.from( dtf.parse( in.nextString() ) );
					
				default:
					throw new IOException( "bad token: "+ in.peek() );
				}
			}
		};
	};
	
	
	public static final Consumer<GsonBuilder> DEFAULT_GSON_BUILDING = new Consumer<GsonBuilder>() {

		@Override
		public void accept(GsonBuilder gb) {
			gb.registerTypeAdapter( LocalDate.class, createLocalDateAdapter("yyyy-MM-dd") );
			gb.registerTypeAdapter( LocalDateTime.class, createLocalDateTimeAdapter("yyyy-MM-dd HH:mm:ss.SSS") );
			gb.setDateFormat( "yyyy-MM-dd HH:mm:ss.SSS" );
			gb.serializeNulls();
			gb.setPrettyPrinting();
		}
	};

	public JsonPersister() {
		this( DEFAULT_GSON_BUILDING );
	}

	/**
	 * 
	 * @param inGsonBulding {@link Consumer} for configuring the GsonBuilder
	 */
	public JsonPersister( Consumer<GsonBuilder> inGsonBulding ) {
		
		inGsonBulding = inGsonBulding == null ? DEFAULT_GSON_BUILDING : inGsonBulding;
		
		final GsonBuilder theGb = new GsonBuilder();
		
		inGsonBulding.accept( theGb );
		
		gson = theGb.create();
		
	}
	
	
	public long save( File inFile, Object inObject ) throws IOException {
		
		Utils.mkParentDirs( inFile );
		
		try (Writer out = new OutputStreamWriter( new FileOutputStream(inFile) , "UTF-8")) {
			
			out.write( toString( inObject ) );
			
			out.flush();
		} 

		return inFile.length();
	}
	
	public <T> T read( File inFile, Class<T> inClass ) throws IOException {
		
		return read( new FileInputStream( inFile ), inClass, true );
	}
	
	public <T> T read(InputStream inIs, Class<T> inClass, boolean inCloseStream) throws IOException {
		
		if ( inIs == null ) { return null; }
		
		try (Reader r = new InputStreamReader(inIs, "UTF-8")) {
			
			return gson.fromJson( r, inClass);
		}
	}
	
	public String getJsonString(Object inObject) {
		return inObject == null ? "null" : gson.toJson( inObject );
	}

	public byte[] getJsonBytes(Object inObject) {
		return getJsonString(inObject).getBytes( UTF8 );
	}

	public String toString(Object inObject) {
		return getJsonString(inObject);
	}
	
}
