package de.greyshine.utils.deprecated;

import java.io.File;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public interface IFileTraverser {

	final static Log LOG = LogFactory.getLog(IFileTraverser.class);

	default boolean start(File inBaseFile) { return true; }

	default void end(int inDirsProcessed, int inFilesProcessed, boolean isCancelled, Exception inLastException) {}

	default boolean handleDirStart(File inDir) throws Exception { return true; }

	default boolean handleDirEnd(File inDir) throws Exception {return true;}

	default boolean handleFile(File inFile) throws Exception {return true;};

	default boolean handleException(File inFile, boolean isStart, boolean isEnd, Exception inException) {throw inException instanceof RuntimeException ? (RuntimeException)inException : new RuntimeException(inException);}

	/**
	 * Traversers only the files in the very first directory
	 */
	public static abstract class FlatDirectoryFilesTraverser extends Traverser {

		public final boolean isQuitOnException;
		private Exception exception = null;
		private boolean isFirstDirectory = true;
		
		public FlatDirectoryFilesTraverser() {

			this(true);
		}

		public FlatDirectoryFilesTraverser(boolean inIsQuitOnException) {

			isQuitOnException = inIsQuitOnException;
		}

		/**
		 * @return last exception thrown
		 */
		@Override
		public Exception getException() {
			return exception;
		}

		@Override
		public boolean handleDirStart(File inFile) throws Exception {

			return isFirstDirectory == false ? false : !(isFirstDirectory = false);
		}

		@Override
		public boolean handleFile(File inFile) throws Exception {

			isFirstDirectory = false;
			return handle(inFile);
		}
		
		/**
		 * @param inFile
		 *            always a file and never a directory
		 * @return
		 * @throws Exception
		 */
		public abstract boolean handle(File inFile) throws Exception;

		@Override
		public boolean handleException(File inFile, boolean isStart, boolean isEnd, Exception inException) {
			exception = inException;
			return isQuitOnException;
		}
	}
	
	public static abstract class Traverser implements IFileTraverser {

		private Exception exception;
		public final boolean isQuitOnException;
		protected boolean isSkipBasedirHandle;

		public Traverser() {

			this(true);
		}

		public Traverser(boolean inIsQuitOnException) {

			isQuitOnException = inIsQuitOnException;
		}

		@Override
		public boolean start(File inBasefile) {
			return true;
		}

		@Override
		public void end(int inDirsProcessed, int inFilesProcessed, boolean isCancelled, Exception inLastException) {
		}

		public Exception getException() {
			return exception;
		}

		@Override
		public boolean handleDirStart(File inFile) throws Exception {
			return true;
		}

		@Override
		public boolean handleDirEnd(File inFile) throws Exception {
			return true;
		}

		@Override
		public boolean handleFile(File inFile) throws Exception {
			return true;
		}

		@Override
		public boolean handleException(File inFile, boolean isStart, boolean isEnd, Exception inException) {

			if (inException == null) {
				return true;
			}

			exception = inException;

			if ( LOG.isDebugEnabled() ) {
				LOG.debug("Exception on traversing [file=" + inFile + ", start=" + isStart + ", end=" + isEnd + ", exception=" + inException.getClass().getName() + "]: " + inException, inException);
			}

			return !isQuitOnException;
		}

		public void travers(File inPath) {

			travers(inPath, false);
		}

		public void travers(File inPath, boolean inThrowRuntimeException) {

			Utils.traversFiles(inPath, this);

			if (inThrowRuntimeException && exception != null) {

				Utils.toRuntimeException(exception);
			}
		}

		public void travers(String inPath) {

			travers(inPath, false);
		}

		public void travers(String inPath, boolean inThrowRuntimeException) {

			Utils.traversFiles(inPath, this);

			if (inThrowRuntimeException && exception != null) {

				Utils.toRuntimeException(exception);
			}
		}
	}

	public static class FilesInfoTraverser extends Traverser {

		private volatile int countDirs = 0;
		private volatile int countDirsUnreadable = 0;
		private volatile int countFiles = 0;
		private volatile int countFilesUnreadable = 0;
		private volatile long bytes = 0;

		public int getCountDirs() {
			return countDirs;
		}

		public int getCountFiles() {
			return countFiles;
		}

		public long getCountBytes() {
			return bytes;
		}

		public int getCountDirsUnreadable() {
			return countDirsUnreadable;
		}

		public int getCountFilesUnreadable() {
			return countFilesUnreadable;
		}

		@Override
		public boolean handleDirStart(File inFile) {

			countDirs++;
			if (!inFile.canRead()) {
				countDirsUnreadable++;
			}
			return true;
		}

		@Override
		public boolean handleFile(File inFile) {
			countFiles++;
			if (!inFile.canRead()) {
				countFilesUnreadable++;
			}

			bytes += inFile.length();

			return true;
		}

		public String getCountBytesReadable() {

			return Utils.getReadableDataSize(bytes);
		}

		@Override
		public String toString() {
			return "FilesInfoTraverser [countDirs=" + countDirs + ", countDirsUnreadable=" + countDirsUnreadable + ", countFiles=" + countFiles + ", countFilesUnreadable=" + countFilesUnreadable + ", bytes=" + bytes + " (" + getCountBytesReadable() + ")]";
		}
	}
}