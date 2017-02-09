package de.greyshine.spielwiese;

import java.io.File;

import de.greyshine.spielwiese.FileChangeObserver.IObserver;

public class Main {
	
	public static void main(String[] args) {
		
		final IObserver o = new IObserver() {

			@Override
			public void event(Event inEvent, File inFile) {
				System.out.println( inEvent + " : "+ inFile );
			}};
		
		final FileChangeObserver fco = new FileChangeObserver( new File("target/watched") );
		fco.addObserver( o );
		
		
		while(true) {
			try {
				Thread.sleep(3000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
	}
	
}
