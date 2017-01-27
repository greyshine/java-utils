package de.greyshine.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

public class CompareToTests {

	@Test
	public void test() {
		
		List<String> l = new ArrayList<>();
		l.add( null );
		l.add( "X1" );
		l.add( null );
		l.add( "X2" );
		
		Collections.sort( l, (a,b)-> Utils.compareToNullFirst(a, b) );
		
		Assert.assertNull( l.get( 0 ) );
		Assert.assertNull( l.get( 1 ) );
		Assert.assertEquals( "X1", l.get( 2 ) );
		Assert.assertEquals( "X2", l.get( 3 ) );
		
		Collections.sort( l, (a,b) -> Utils.compareToNullLast(a, b) );
		
		Assert.assertEquals( "X1", l.get( 0 ) );
		Assert.assertEquals( "X2", l.get( 1 ) );
		Assert.assertNull( l.get( 2 ) );
		Assert.assertNull( l.get( 3 ) );
		
		
	}
	
}
