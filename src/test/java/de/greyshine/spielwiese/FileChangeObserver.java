package de.greyshine.spielwiese;

import java.io.File;
import java.io.IOException;
import java.nio.file.ClosedWatchServiceException;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.StandardWatchEventKinds;
import java.nio.file.WatchEvent;
import java.nio.file.WatchEvent.Kind;
import java.nio.file.WatchKey;

import static java.nio.file.StandardWatchEventKinds.*;
import java.nio.file.WatchService;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import de.greyshine.spielwiese.FileChangeObserver.IObserver.Event;
import de.greyshine.utils.Utils;
import de.greyshine.utils.Utils.ILogger;

/**
 * http://stackoverflow.com/a/27737069/845117
 */
public class FileChangeObserver {
	
	public interface IObserver {
		
		enum Event {
			DIR_INIT,
			DIR_CREATED,
			DIR_MODIFIED,
			DIR_DELETED,
			FILE_INIT,
			FILE_CREATED,
			FILE_MODIFIED,
			FILE_DELETED
		}
		
		void event(Event inEvent, File inFile);
		
		default void stop() {};
	}
	
	public static final IFileFilter ALLOW_ALL = new IFileFilter(){};
	
	private final List<IObserver> observers = new ArrayList<>(1);
	private IFileFilter fileFilter = ALLOW_ALL;
	private final WatchService watchService;
	private File watchedDir;
	private File file;
	private Map<File, WatchKey> directoryWatchKeys = new HashMap<>();
	private Map<File, List<File>> filesInDirectory = new HashMap<>();
	
	private final Threadrun threadrun = new Threadrun();
	private ILogger logger = null;
	
	public interface IFileFilter {
		default boolean isIgnored(File inFile) { return false; }
	}
	
	public FileChangeObserver(File inFile) {
		
		file = Utils.getCanonicalFile( inFile );
		
		if ( file == null ) { throw new IllegalArgumentException("file must not be null"); }

		try {
		
			watchedDir = (file.isDirectory() ? inFile: inFile.getParentFile()).getCanonicalFile();
			
			watchService = FileSystems.getDefault().newWatchService();
		
		} catch (IOException e) {
			
			throw new IllegalStateException("Cannot instantiate Object of "+ getClass() +": "+e.getMessage(), e );
		}
		
	}
	
	public void start() {
		
		if ( threadrun.isRunning ) { throw new IllegalStateException("already started"); }
		
		threadrun.start();
		
		Utils.threadWait( threadrun.isRunning, 2000L, ()->{ return !threadrun.isRunning; }  );
	}
	
	public FileChangeObserver logger(ILogger inLogger) {
		logger = inLogger;
		return this;
	}
	
	private void log(Object inMessage, Throwable t) {
		Utils.executeQuietly( "["+ getClass().getSimpleName() +"] "+ inMessage, (msg)->{ logger.log(msg, t); });
	}
	
	private void logErr(Object inMessage, Throwable t) {
		Utils.executeQuietly( "["+ getClass().getSimpleName() +"] "+ inMessage, (msg)->{ logger.err(msg, t); });
	}
	
	public FileChangeObserver fileFilter(IFileFilter inFileFilter) {
		fileFilter = inFileFilter == null ? ALLOW_ALL : inFileFilter;
		return this;
	}
	
	public FileChangeObserver addObserver( IObserver inObserver ) {
		
		if ( inObserver != null ) {
			observers.add( inObserver );
		}
		
		return this;
	}
	
	public List<IObserver> getObservers() {
		return new ArrayList<>( observers );
	}
	
	public void removeObserver(IObserver inObserver) {
		observers.remove( inObserver );
	}
	
	public static FileChangeObserver create( File inFile, IObserver inObserver ) {
		return new FileChangeObserver(inFile).addObserver(inObserver);
	}
	
	public void stop() {

		try {
			
			watchService.close();
		
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		synchronized ( observers ) {
			while( !observers.isEmpty() ) {
				Utils.executeQuietly( observers.remove(0), (inObserer)->{ inObserer.stop(); } );
			}
		}
	}
	
	private void registerWatchKeysForDir(File inDir) throws IOException {
		
		final WatchKey theWatchKey = inDir.toPath().register( watchService ,ENTRY_CREATE, ENTRY_MODIFY, ENTRY_DELETE, OVERFLOW);
		final WatchKey theOldWatchKey = directoryWatchKeys.put( inDir , theWatchKey);
		
		if ( theWatchKey == theOldWatchKey || theWatchKey.equals( theOldWatchKey ) ) {
			
			logErr("check code for watchkey handling",null );
			
		} else if ( theOldWatchKey != null ) { 
			
			theOldWatchKey.cancel();
		}
		
		log("INFO: "+ "registered dir: "+ inDir, null);
		
	}
	
	private void scanInitially() {
		
		Utils.traversFiles(file, (f)->{
			
			Event event = Utils.isFile( f ) ? Event.FILE_INIT : Utils.isDir( f ) ? Event.DIR_INIT : null;
			
			if ( event == null ) { return true; }
				
			try {
				
				fireEvent( event, file );

			} catch (Exception e) {

				e.printStackTrace();
			}
			
			return true;
			
		}, true);
		
	}
	
	private void fireEvent(Event inEvent, File inFile) {

		try {
			
			if ( fileFilter.isIgnored( inFile ) ) { return; }
			
		} catch (Exception e) {
			
			e.printStackTrace();
			return;
		}
		
		new ArrayList<>( observers ).stream().forEach( (anObserver)->{
			
			try {
				
				anObserver.event(inEvent, inFile);
				
			} catch (Exception e) {
				
				logErr( e, null );
			}

		} );
	}

	private class Threadrun extends Thread {
		
		private boolean isRunning = false;
		
		@Override
		public void run() {
			
			scanInitially();
			
			try {
				
				registerWatchKeysForDir( watchedDir );
				
			} catch (IOException e1) {
			
				e1.printStackTrace();
				
				logErr("exit", null);
				System.exit(1);
				return;
			}
			
			isRunning = true;
			
			Utils.threadNotifyAll( isRunning );
			
			log("started watching dir: "+ watchedDir, null );
			
			while( isRunning ) {
				
				try {
					
					log( "waiting for java.nio.file.WatchEvents ...", null );
					
					final WatchKey wk = watchService.take();
					
					log( "new Event: "+ LocalDateTime.now() +" :: "+wk, null );
					
					final List<WatchEvent<?>> theWes = wk.pollEvents(); 
					
					final boolean isValid = wk.reset();
					
					theWes.stream().forEach( (we)->{
						
						@SuppressWarnings("unchecked")
						final Kind<?> theKind = (Kind<Path>) we.kind();
						final Path thePath = (Path) we.context();
						final File theFile = watchedDir.toPath().resolve( thePath ).toFile();
						
						if ( theKind.name().equals( StandardWatchEventKinds.OVERFLOW.name() ) ) {
							System.out.println( "OVERFLOW: "+ theFile );
							return;
						}
						
						log( "WatchEvent: "+ theKind, null );
						log( "\tfile: "+ theFile, null );
						
						final Event theEvent = evaluateEvent( theKind, theFile );
						log( "\tWatchEvent as FileChangeObserver.Event: "+ theEvent, null );
						
						switch ( theEvent ) {
						case DIR_CREATED:
							
							final List<File> theNewDirs = new ArrayList<>();
							
							Utils.traversFiles( theFile, (aFile)->{ 
							
								if ( Utils.isDir( aFile ) ) {
								
									fireEvent( Event.DIR_CREATED, aFile);
									theNewDirs.add( aFile );
								
								} else if ( Utils.isFile( aFile ) ) {
									
									fireEvent( Event.FILE_CREATED, aFile);
								}
								
								return true;
							
							}, true );
							
							theNewDirs.forEach( (aDir)->{
								try {
									registerWatchKeysForDir( theFile );
								} catch (IOException e) {
									// TODO
									e.printStackTrace();
								}
							} );
							
							
							break;

						case DIR_DELETED:
							
							final WatchKey theWk = directoryWatchKeys.get( theFile );
							log("deleted watchkey: "+ theFile +" "+ theWk, null );
							
							break;
							
						default:
							fireEvent( theEvent, theFile );
							break;
						}
						
						
					} );
				
				} catch (ClosedWatchServiceException e) {
				
					isRunning = false;
					
				} catch (Exception e) {
					
					e.printStackTrace();
				
				}
			}
			
			log( "exit watching: "+ watchedDir, null );
		}
	}
	
	private Event evaluateEvent(Kind<?> inKind, File inFile) {
		
		final boolean isDir = Utils.isDir( inFile );
		if ( isDir && inKind.name().equals( StandardWatchEventKinds.ENTRY_CREATE.name() ) ) {
			return Event.DIR_CREATED;
		}
		if ( isDir && inKind.name().equals( StandardWatchEventKinds.ENTRY_MODIFY.name() ) ) {
			return Event.DIR_MODIFIED;
		}
		final boolean isWatchedDir = directoryWatchKeys.containsKey(inFile );
		if ( isWatchedDir && inKind.name().equals( StandardWatchEventKinds.ENTRY_DELETE.name() ) ) {
			return Event.DIR_DELETED;
		}
		
		final boolean isFile = Utils.isFile( inFile );
		if ( isFile && inKind.name().equals( StandardWatchEventKinds.ENTRY_CREATE.name() ) ) {
			return Event.FILE_CREATED;
		}
		if ( isFile && inKind.name().equals( StandardWatchEventKinds.ENTRY_MODIFY.name() ) ) {
			return Event.FILE_MODIFIED;
		}
		if ( inKind.name().equals( StandardWatchEventKinds.ENTRY_DELETE.name() ) ) {
			return Event.FILE_DELETED;
		}
		
		throw new IllegalStateException("unreachable code or bug. Ok, its a bug.");
	}
}
