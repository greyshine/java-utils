package de.greyshine.spielwiese;

import java.io.File;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JLabel;

public class ShowFile {

	public static void main(String[] args) throws IOException {
		
		args = args == null || args.length < 1 ? new String[] { "default:"+new File(".").getCanonicalPath() } : args;
		
		System.out.println( args[0] );
		
		final JFrame jf = new JFrame( "WORKS" );
		jf.setSize( 300 , 300);
		jf.getContentPane().add( new JLabel( args[0] ) );
		jf.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
		jf.setVisible(true);
	}
	
}
