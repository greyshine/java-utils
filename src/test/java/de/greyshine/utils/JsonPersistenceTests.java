package de.greyshine.utils;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Date;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.beta.JsonPersister;

public class JsonPersistenceTests {

	@Test
	public void test() throws IOException {
		
		final File theFile = new File("./target/"+ JsonPersistenceTests.class.getName()+"/test.json");
		
		theFile.delete();
		
		Assert.assertFalse( Utils.isFile( theFile ) );
		
		final Object o = new Someclass();
		final String s1 = o.toString();
		
		final JsonPersister jp = new JsonPersister(  );
		
		System.out.println( jp.toString( o ) );
		
		jp.save(theFile, o);
	
		Someclass o2 = jp.read( theFile, Someclass.class );
	
		System.out.println("localDate: "+ o2.ld );
		
		Assert.assertEquals(257L, theFile.length());
		
		System.out.println( o.toString() );
		System.out.println( o2.toString() );
		
		// TODO 
		//Assert.assertEquals( o.toString(), o2.toString() );
		
	}
	
	public static class Someclass {
		
		String text = Someclass.class.getTypeName();
		BigDecimal number = new BigDecimal( "47.11" );
		boolean yesOrNo = true;
		Object nullObject = null;
		Date date = new Date();
		int[] nums = new int[] {1,2,3};
		transient Object object = new Object();
		LocalDateTime ldt = LocalDateTime.now();
		LocalDate ld = LocalDate.now();
		@Override
		public String toString() {
			return "Someclass [text=" + text + ", number=" + number + ", yesOrNo=" + yesOrNo + ", nullObject="
					+ nullObject + ", date=" + date + ", nums=" + Arrays.toString(nums) + ", object=" + object
					+ ", ldt=" + ldt + ", ld=" + ld +"]";
		}
	}
	
	
}
