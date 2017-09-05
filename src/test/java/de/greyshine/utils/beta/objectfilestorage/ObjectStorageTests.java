package de.greyshine.utils.beta.objectfilestorage;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import de.greyshine.utils.beta.objectfilestorage.Address.EType;

public class ObjectStorageTests {
	
	private IObjectStorage ofs;
	
	
	@Before
	public void beforeTest() throws IOException {
		ofs = new ObjectStorage( "target"+File.separatorChar+ ObjectStorage.class.getSimpleName() );
	}
	
	@After
	public void afterTest() throws IOException {
		ofs.close();
	}
	
	@Test
	public void testWriteRead() throws IOException {
		
		final User theUser = new User();
		theUser.login = "admin";
		theUser.password = "1234";
		theUser.addresses.put( EType.EMAIL , new Address().address( "some@where.com" ) );
		theUser.addresses.put( EType.WWW , new Address().address( "ww.where.com" ) );
		
		String theId = ofs.save( theUser );
		
		assertNotNull( theId );
		assertNotNull( theUser.id );
		assertEquals( theId, theUser.id );
		
		User theUser2 = ofs.read(User.class, theId);
		
		assertEquals( theUser , theUser2);
	} 

	@Test
	@Ignore
	public void testFilter() throws IOException {
		
		User theUser = new User();
		theUser.login = "admin";
		ofs.save( theUser );
		
		final String pwdString = UUID.randomUUID().toString();
		final long count = ofs.countEntries();
		final int adds = 4;
		
		for( int i = 1; i <= adds/2; i++ ) {
		
			theUser = new User();
			theUser.login = "user-"+ UUID.randomUUID().toString();
			theUser.password = "1234";
			ofs.save(theUser);
		}
		
		
		for( int i = 1; i <= adds/2; i++ ) {
			
			theUser = new User();
			theUser.login = "user-"+ UUID.randomUUID().toString();
			theUser.password = pwdString;
			ofs.save(theUser);
		}
		
		assertEquals( count+adds, ofs.countEntries() );
		
		final List<User> theFilteredUsers = ofs.filter(User.class, (inUser)->{
			
			return pwdString.equals( inUser.password );
		});
		
		assertEquals( adds/2 , theFilteredUsers.size());
		
	} 
	
	
	
}
