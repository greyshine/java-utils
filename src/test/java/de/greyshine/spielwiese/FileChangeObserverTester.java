package de.greyshine.spielwiese;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import de.greyshine.spielwiese.FileChangeObserver.IObserver;
import de.greyshine.spielwiese.FileChangeObserver.IObserver.Event;
import de.greyshine.utils.Utils;

@Ignore
public class FileChangeObserverTester {
	
	final File watchedDir = Utils.getCanonicalFile( "target/tests-"+ FileChangeObserver.class.getSimpleName(), true);  
	
	private Observer observer = new Observer();
	private FileChangeObserver fco;
	
	@Before
	public void before() {
		
		Utils.delete( watchedDir );
		
		Assert.assertFalse( watchedDir.exists() );
		
		watchedDir.mkdirs();
		
		Assert.assertTrue( Utils.isDir( watchedDir ) );
		
		fco  = new FileChangeObserver( watchedDir );
		fco.logger( Utils.ILogger.SYSTEM_OUT_ERR );
		fco.addObserver( observer );
		
		System.out.println( "time: "+ LocalDateTime.now() );
		
	}
	
	@After
	public void after() {
		System.out.println( "test after()..." );
		fco.stop();
	}

	@Test
	public void test() throws IOException {
		
		observer.next( Event.DIR_INIT, watchedDir);
		fco.start();
		
		File f = new File( watchedDir, "f1" );
		
		observer.next( Event.FILE_CREATED, f);
		Assert.assertTrue( f.createNewFile() );

		File d1 = new File( watchedDir, "d1" );
		File d2 = new File( d1, "d2" );
		f = new File( d2, "f2" );
		observer.next( Event.DIR_CREATED, d1);
		observer.next( Event.DIR_CREATED, d2);
		observer.next( Event.FILE_CREATED, f);
		
		Assert.assertTrue( d2.mkdirs() );
		Assert.assertTrue( f.createNewFile() );
		
		observer.waitEvents();
	}
	
	private class Observer implements IObserver {
		
		final Object SYNC = new Object();
		final List<Event> events = new ArrayList<>();
		final List<File> files = new ArrayList<>();
		final List<Long> maxTimes = new ArrayList<>();
		
		void next( Event e, File f) {
			
			synchronized ( SYNC ) {
				
				events.add( e );
				files.add( f );
				long maxTime = System.currentTimeMillis() + Utils.MILLIS_1_SECOND*10;
				maxTimes.add( maxTime );
				
				System.out.println( "registered next event "+ e +" on file "+ f +" until "+ new Date( maxTime ) );
			}
		}
		
		void waitEvents() {
			
			System.out.println( "Test is waiting for "+ maxTimes.size() +" events..." );
			
			while( !maxTimes.isEmpty() ) {
				
				synchronized ( SYNC ) {
					
					Utils.threadSleep( 500 );
					
					final long now = System.currentTimeMillis();
					
					for( int i=0; i < events.size(); i++ ) {
						
						final long timeToWait = maxTimes.get(i) - now;
					
						Assert.assertTrue( "Timeout "+ timeToWait +" for waiting event: "+ events.get( i ) +" on file "+ files.get( i ), timeToWait > -1 );
					}
				}
			}
			
			System.out.println( "no more waiting for events." );
		}
		
		@Override
		public synchronized void event(Event inEvent, File inFile) {
			
			
			synchronized ( SYNC  ) {
				
				System.out.println( "Test-Checking event: "+ inEvent +" on "+inFile +"..." );

				for(int i = 0; i < files.size() ; i++) {
					
					if ( files.get( i ).equals( inFile ) && inEvent == events.get(i) ) {
						
						events.remove(i);
						files.remove(i);
						maxTimes.remove(i);
						
						System.out.println( "... Test-Checking event: "+ inEvent +" on "+inFile +". OK." );
					}
				}
			}
		}
	}
	
	
}
