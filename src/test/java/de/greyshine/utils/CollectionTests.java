package de.greyshine.utils;

import java.util.ArrayList;
import java.util.List;

import org.junit.Assert;
import org.junit.Test;

public class CollectionTests {

	@Test
	public void testGetIndexedListArg() {
		
		List<String> l = new ArrayList<>();
		
		Assert.assertNull( Utils.getIndexedValueSafe(l, 0, null) );
		l.add( "a" );
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(l, 0, null) );
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(l, -1, null) );
		
		
		l.add( "b" );
		l.add( "c" );
		Assert.assertEquals( "c", Utils.getIndexedValueSafe(l, -1, null) );
		Assert.assertEquals( "b", Utils.getIndexedValueSafe(l, -2, null) );
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(l, -3, null) );
	}

	@Test
	public void testGetIndexedArrayArg() {
		
		String[] a = new String[]{};
		
		Assert.assertNull( Utils.getIndexedValueSafe(a, 0, null) );
		a = new String[]{"a"};
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(a, 0, null) );
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(a, -1, null) );
		
		
		a = new String[]{"a","b","c"};
		Assert.assertEquals( "c", Utils.getIndexedValueSafe(a, -1, null) );
		Assert.assertEquals( "b", Utils.getIndexedValueSafe(a, -2, null) );
		Assert.assertEquals( "a", Utils.getIndexedValueSafe(a, -3, null) );
	}
}
