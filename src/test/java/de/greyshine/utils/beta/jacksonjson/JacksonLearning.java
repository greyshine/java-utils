package de.greyshine.utils.beta.jacksonjson;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.fasterxml.jackson.annotation.JsonView;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;


public class JacksonLearning {
	
	@Test
	public void run() throws IOException {
		
		Some s = new Some();
		s.someNumbers.put( "one" , new BigDecimal( "1.11" ));
		s.someNumbers.put( "two" , new BigDecimal( "2.22" ));
		
		System.out.println( stream(s) );
	}
	
	public String stream(Object inObject) throws IOException {

		ObjectMapper om = new ObjectMapper();
		om.writerWithDefaultPrettyPrinter();
		om.enable(SerializationFeature.INDENT_OUTPUT);
		

		ByteArrayOutputStream theBaos = new ByteArrayOutputStream();
		
		om.writeValue( theBaos , inObject);
		
		return new String( theBaos.toByteArray() );
	}
	
	public static class Some {

		@JsonView()
		String text;
		
		@JsonView(  )
		List<Integer> ints = Arrays.asList( 12, null, 13 );
		
		@JsonView
		Foo foo = new Foo();
		
		@JsonView
		Map<String,BigDecimal> someNumbers = new HashMap<>(); 
		
	}
	
	public static class Foo {
		
		@JsonView
		String aha = "works";
		
	}

		
}
