package de.greyshine.tools;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.security.MessageDigest;
import java.util.Arrays;

import de.greyshine.utils.CommandLineParser;
import de.greyshine.utils.CommandLineParser.Args;
import de.greyshine.utils.Utils;

public class Trnsfrm {

	static final CommandLineParser CLP = new CommandLineParser();

	static final String OPTION_COPY2CLIPBOARD = "c2c";
	
	static {

		CLP.headerText("By default tool reads from stdin and writes to stdout.");

		CLP.simpleArg("algorithm")
				.description("The algorithm to apply.\nOne of the values: " + Arrays.asList(Algorithm.values()));
		CLP.option("f", "file").parameter("file").optional().description("read file instead stdin");
		CLP.option("t", "text").parameter("text").optional().description("read given text instead stdin");
		CLP.option("c", "readclipboard").optional().description("read from clipboard instead stdin");
		CLP.option(OPTION_COPY2CLIPBOARD, "copy2clipboard").optional()
				.description("copy output to clipboard\n(might not be supported by OS)");
		// TODO...
		//CLP.option("wh", "wraphtml").optional().description("wrap html img-tag around output.\n(Will only apply when <algorithm is " + Algorithm.BASE64 + ">)");
		//CLP.option("whit", "imagetype").parameter("image-type").optional().description("wrap html img-tag around output.\n(Will only apply when <algorithm is " + Algorithm.BASE64 + ">)");
		CLP.option("o", "out").parameter("file").optional().description("write output to given file");
		CLP.optionVerbose().optionQuiet();
		CLP.option("h", "help").optional().description("show this message");
		CLP.generateUsageText("trnsfrm");
	}

	private final Args args;
	private final boolean isQuiet, isVerbose;
	private final Algorithm algorithm;
	private final InputStream inStream;
	private final OutputStream outStream;
	
	
	private Trnsfrm(Args inArgs) {
		
		this.args = inArgs;
		
		isQuiet = args.isQuiet();
		isVerbose = args.isVerbose();
		
		final String theAlgorithmName = args.getSimpleArg(0); 
		
		algorithm = Algorithm.lookup(theAlgorithmName);
		
		if (algorithm == null) {
			
			inStream = null;
			outStream = null;

			serr("bad algorithm: " + theAlgorithmName + "; " + Arrays.asList(Algorithm.values()), false);
			return;
		}
		
		sout("algorithm: "+ algorithm, true);
		
		inStream = createInputStream();
		outStream = createOutputStream();
	}
	
	private void execute() throws Exception {
		
		if ( inStream == null ) { return; }
		if ( outStream == null ) { return; }
		
		if (inStream == System.in) {
			sout( "waiting for input on stdin...", true);
		}
		
		algorithm.handle( inStream , outStream);
		
	}

	public static void main(String[] args) throws Exception {

		new Trnsfrm( CLP.parse(args) ).execute();
	}

	public InputStream createInputStream() {
		
		int count = 0;
		
		InputStream theIs = System.in;
		String textInputStreamUsed = "stdin";

		final File theInputFile = isReadFromFile();

		if (theInputFile != null) {

			count++;

			try {
				
				theIs = Utils.isNoFile( theInputFile ) ? null : new FileInputStream(theInputFile);
				textInputStreamUsed = "file; "+ Utils.getCanonicalFile( theInputFile ) +" ("+ Utils.formatDataSize( theInputFile.length() ) +")";
				
			} catch (FileNotFoundException e) {

				serr( "cannot read from file: " + theInputFile, false);
				return null;
			}
		}

		final String theText = theIs != null ? null : isReadFromTextArg();

		if (theText != null) {

			count++;
			theIs = theText.isEmpty() ? null : new ByteArrayInputStream(theText.getBytes());
			textInputStreamUsed = "textparameter";
		}

		if ( theIs == null && isReadFromClipboard()) {
			
			count++;
			// from
			// http://www.avajava.com/tutorials/lessons/how-do-i-get-a-string-from-the-clipboard.html
			try {

				final Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
				Transferable t = clipboard.getContents(null);
				final DataFlavor[] theDfs = t.getTransferDataFlavors();
				if (theDfs != null && theDfs.length > 0) {

					Object theData = t.getTransferData(theDfs[0]);

					sout( "clipboard datatype: " + (theData == null ? "<null>" : theData.getClass().getCanonicalName()), true);

					if (theData instanceof InputStream) {

						theIs = (InputStream) theData;

					} else if (theData != null) {

						theIs = new ByteArrayInputStream(theData.toString().getBytes());

					} else {

						theIs = null;
					}
				}
				
				textInputStreamUsed = "clipboard";

			} catch (Exception e) {

				serr( "failed to read clipboard: " + e, false);
				return null;

			}
		}

		if (count > 1) {
			serr( "no unique input source", false);
			return null;
		}
		
		sout("reading from: "+ textInputStreamUsed, true);

		return theIs;
	}

	private boolean isReadFromClipboard() {
		return args.isOption( "c" );
	}

	public OutputStream createOutputStream() {

		OutputStream os = System.out;
		final File theOutfile = getFileOutput();

		if (theOutfile != null) {

			try {

				os = new FileOutputStream(theOutfile);

			} catch (FileNotFoundException e) {

				serr( "unable to write file: " + theOutfile, false);
				return null;
			}
		}

		if (isCopyToClipboard()) {

			sout("copy result to clipboard", true);

			os = new CbOutputStream(os);
		}

		return os;
	}

	private boolean isCopyToClipboard() {
		return args.isOption(OPTION_COPY2CLIPBOARD);
	}

	private File getFileOutput() {
		return args.getOptionParameterAsFile("o");
	}

	private void sout(String inMsg, boolean isVerboseMessage) {
		if (isVerboseMessage && !isVerbose) {
			return;
		} else if (!isQuiet) {
			System.out.println(inMsg);
		}
	}

	private void serr(String inMsg, boolean isVerboseMessage) {
		if (isVerboseMessage && !isVerbose) {
			return;
		} else if (!isQuiet) {
			System.err.println(inMsg);
		}
	}

	enum Algorithm {

		BASE64 {

			@Override
			void handle(InputStream is, OutputStream out) throws Exception {
				Utils.streamBase64(is, out);
			}
		},
		MD5 {

			@Override
			void handle(InputStream is, OutputStream out) throws Exception {

				final MessageDigest md = MessageDigest.getInstance("MD5");

				int r;
				while ((r = is.read()) > -1) {

					md.update((byte) r);
				}

				String hashtext = new BigInteger(1, md.digest()).toString(16);
				// Now we need to zero pad it if you actually want the full 32
				// chars.
				while (hashtext.length() < 32) {
					hashtext = "0" + hashtext;
				}

				out.write(hashtext.getBytes());
			}
		},
		SHA1 {

			@Override
			void handle(InputStream is, OutputStream out) throws Exception {

				final MessageDigest md = MessageDigest.getInstance("SHA-1");

				int r;
				while ((r = is.read()) > -1) {

					md.update((byte) r);
				}

				String hashtext = new BigInteger(1, md.digest()).toString(16);
				// Now we need to zero pad it if you actually want the full 32
				// chars.
				while (hashtext.length() < 32) {
					hashtext = "0" + hashtext;
				}

				out.write(hashtext.getBytes());
			}

		},
		SHA256 {

			@Override
			void handle(InputStream is, OutputStream out) throws Exception {

				final MessageDigest md = MessageDigest.getInstance("SHA-256");

				int r;
				while ((r = is.read()) > -1) {

					md.update((byte) r);
				}

				String hashtext = new BigInteger(1, md.digest()).toString(16);
				// Now we need to zero pad it if you actually want the full 32
				// chars.
				while (hashtext.length() < 32) {
					hashtext = "0" + hashtext;
				}

				out.write(hashtext.getBytes());
			}
		},
		TEST {

			@Override
			void handle(InputStream is, OutputStream out) throws Exception {
				int r;
				while ((r = is.read()) > -1) {
					out.write(r);
				}
			}
		};

		void handle(InputStream is, OutputStream out) throws Exception {
			System.err.println("not implemented: " + name());
			System.exit(1);
		}

		void done() {
		}

		static Algorithm lookup(String name) {

			for (Algorithm a : values()) {
				if (a.name().equalsIgnoreCase(name)) {
					return a;
				}
			}

			return null;
		}
	}
	
	private File isReadFromFile() {
		return args.getOptionParameterAsFile( "f" );
	}
	
	private String isReadFromTextArg() {
		return args.getOptionParameter( "t", null );
	}
	
    private class CbOutputStream extends OutputStream  {
    	
    	final StringBuilder text = new StringBuilder();
    	final OutputStream os;
		
		public CbOutputStream(OutputStream os) {
			this.os = os;
		}

		@Override
		public void write(int b) throws IOException {
			
			text.append( (char)b );
			
			os.write(b);
		}

		@Override
		public void flush() throws IOException {
			os.flush();
		}

		@Override
		public void close() throws IOException {
			
			final String theText = text.toString();

			if ( !theText.trim().isEmpty() ) {
                
				try {
					Toolkit.getDefaultToolkit().getSystemClipboard().setContents( new StringSelection( theText ), null);
					
					sout("copied "+ theText.length() +" bytes to clipboard", true);
					
				} catch (Exception e) {
					serr( e.getMessage(), false);
				}
            }
			
			os.flush();
			
			if ( os != System.out ) {
				os.close();
			}
		}
    }
}
