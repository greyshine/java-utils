package de.greyshine.utils;

import java.math.BigDecimal;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.beta.Mapper;
import de.greyshine.utils.beta.Mapper.MappingException;

public class MapperTests {
	
	final BigDecimal BD_47_11 = new BigDecimal("47.11");
	
	@Test
	public void test() throws MappingException {
		
		final Mapper m = new Mapper();
		
		final BigDecimal d = m.map( "47.11" , BigDecimal.class);
		Assert.assertEquals( 0 , BD_47_11.compareTo( d ));

		int i = m.map( "47" , Integer.class); 
		Assert.assertEquals( 47 , i );
	}

}
