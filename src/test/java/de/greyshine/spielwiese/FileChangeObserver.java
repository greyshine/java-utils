package de.greyshine.spielwiese;

import java.io.File;
import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.WatchService;
import java.util.ArrayList;
import java.util.List;

import de.greyshine.utils.Utils;

/***
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
	private File watchedFile;
	
	public interface IFileFilter {
		default boolean isIgnored(File inFile) { return false; }
	}
	
	public FileChangeObserver(File inFile) {
		
		if ( (watchedFile = inFile) == null ) { throw new IllegalArgumentException("file must not be null"); }
		
		try {
			watchService = FileSystems.getDefault().newWatchService();
		} catch (IOException e) {
			throw new IllegalStateException("Cannot instantiate Object of "+ getClass() +": "+e.getMessage(), e );
		} 
		
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

		synchronized ( observers ) {
			while( !observers.isEmpty() ) {
				Utils.executeQuietly( observers.remove(0), (inObserer)->{ inObserer.stop(); } );
			}
		}
		
		try {
			
			watchService.close();
		
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
