package de.greyshine.utils;

import org.junit.Assert;
import org.junit.Test;

import de.greyshine.utils.Wrapper;

public class UtilsTests {
	
	final Object o = new Object() {
		final long time = System.currentTimeMillis();
		@Override
		public String toString() {
			return super.toString() + "["+ time +"]";
		}
	};
	
	@Test
	public void testUtilsWrapper() {
		
		final Wrapper<Object> w = Utils.wrapper( o );
		Assert.assertTrue( w.value == o );
		Assert.assertTrue( w.isNotNull() );
		
		final int hash = w.map( v->{ return v.hashCode(); } );
		Assert.assertEquals( o.hashCode(),  hash );
		
		final Wrapper<Object> result = Utils.wrapper();
		w.consume( v -> { result.set( v ); } );
		
		Assert.assertEquals( o, result.value );
		
		
		
	}

}
