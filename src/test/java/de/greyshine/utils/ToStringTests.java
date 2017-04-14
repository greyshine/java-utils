package de.greyshine.utils;

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.util.function.Function;

import org.junit.Assert;
import org.junit.Test;

public class ToStringTests {

	final ToString tostring = new ToString();
	
	@Test
	public void test() {
		
		Assert.assertEquals( "Hello World!" , tostring.toString( "Hello World!" ) );
		Assert.assertEquals( "47.11" , tostring.toString( new BigDecimal("47.11") ) );
		Assert.assertEquals( "47.11" , tostring.toString( 47.11 ) );
		Assert.assertEquals( "47.11" , tostring.toString(new Double( 47.11 ) ));
		
	}
	
	@Test
	public void testThrowables() {

		Assert.assertEquals( "java.lang.NullPointerException [message=null]" , Utils.toString( new NullPointerException() ) );
	
		final UnsupportedOperationException uoe = new UnsupportedOperationException( "Message!" , new NullPointerException("Nada"));
		
		Assert.assertEquals( "java.lang.UnsupportedOperationException [message=Message!, cause=java.lang.NullPointerException]" , Utils.toString( uoe ) );
	}
	
	@Test
	public void testArray() {
		
		final String[] theStrings = {"One",null,"Three"};
		System.out.println( theStrings.getClass() );
		Assert.assertEquals( "3:[One, null, Three]" , Utils.toString( theStrings ) );
		
		final int[] theInts = {1,2,3};
		Assert.assertEquals( "3:[1, 2, 3]" , Utils.toString( theInts ) );
		
		final boolean[] theBools = {};
		Assert.assertEquals( "0:[]" , Utils.toString( theBools ) );
	}
	
	@Test
	public void testInheritance() {
		
		String theTxt = tostring.toString( tostring );
		Assert.assertTrue(theTxt, Utils.isMatch(  theTxt , "de\\.greyshine\\.utils\\.ToString \\[hash=\\-?[0-9]+\\]") );

		tostring.register( A.class , new Function<Object,String>(){
			public String apply(Object t) {
				return "A.class-toString()";
			}
		});
		
		final A a = new A();
		String toString_a = tostring.toString( a );
		Assert.assertEquals( "A.class-toString()" , toString_a);
		
		final B b = new B();
		
		String toString_b = tostring.toString( b );
		//System.out.println( toString_b );
		Assert.assertEquals( "A.class-toString()" , toString_b);

		tostring.register( B.class , new Function<Object,String>(){
			public String apply(Object t) {
				return "B.class-toString()";
			}
		});
		
		toString_b = tostring.toString( b );
		//System.out.println( toString_b );
		Assert.assertEquals( "B.class-toString()" , toString_b);
	}
	
	static class A {
		
	}
	
	static class B extends A {
		
	}
	
}
