package de.greyshine.utils.deprecated;

import java.io.BufferedReader;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class CsvReader {

	private final boolean isFirstLineHeadings;
	private List<String> headings = null;
	private Pattern separatorPattern;
	private boolean isTrimLines = true;
	private boolean isTrimCells = true;
	private boolean isCellWrapped = false;
	
	public CsvReader(String inSeparatorPattern) {

		this(inSeparatorPattern, false, false, null);
	}

	public CsvReader(String inSeparatorPattern, boolean isCellWrapped, boolean inFirstLineHeadings) {

		this(inSeparatorPattern, isCellWrapped, inFirstLineHeadings, null);
	}

	public CsvReader(String inSeparatorPattern, boolean isCellWrapped, boolean inFirstLineHeadings, String[] inHeadings) {

		this(inSeparatorPattern, isCellWrapped, true, true,  inFirstLineHeadings, inHeadings);
	}

	public CsvReader(String inSeparatorPattern, boolean isCellWrapped, boolean isTrimLines, boolean isTrimCells, boolean inFirstLineHeadings, String[] inHeadings) {

		separatorPattern = Pattern.compile(inSeparatorPattern);
		isFirstLineHeadings = inFirstLineHeadings;
		setHeadings(inHeadings);
		this.isTrimCells = isTrimCells;
		this.isTrimLines = isTrimLines;
		this.isCellWrapped = isCellWrapped;
	}

	public void setHeadings(String[] inHeadings) {

		if (inHeadings == null || inHeadings.length == 0) {

			headings = null;
			return;
		}

		for (final String aHeading : inHeadings) {

			headings.add(aHeading == null ? "" : aHeading);
		}
	}

	public boolean isTrimLines() {
		return isTrimLines;
	}

	public void setTrimLines(boolean isTrimLines) {
		this.isTrimLines = isTrimLines;
	}

	public boolean isTrimCells() {
		return isTrimCells;
	}

	public void setTrimCells(boolean isTrimCells) {
		this.isTrimCells = isTrimCells;
	}

	public void read(InputStream in, String inCharset, final ICellHandler inCellHandler) throws Exception {

		if (inCellHandler == null) {

			throw new IllegalArgumentException("ICellHandler must not be null.");
		}

		read(in, inCharset, new IRowHandler() {

			int lastRowIndex = -1;

			@Override
			public boolean begin(List<String> inHeadings) {
				return inCellHandler.begin(inHeadings);
			}

			@Override
			public boolean handleRow(int inRowIndex, Map<Integer, String> inValuesByIndex, Map<String, String> inValuesByName, Integer inHeadingDifference, String inLine) throws Exception {

				boolean isContinue = true;

				if (lastRowIndex != inRowIndex) {

					if (!inCellHandler.handleNewline(inRowIndex, inLine)) {
						return false;
					}
				}

				lastRowIndex = inRowIndex;

				final int len = inValuesByIndex.size();

				for (int i = 0; i < len && isContinue; i++) {
					
					isContinue &= inCellHandler.handleCell(inRowIndex, headings == null ? String.valueOf(i) : headings.size() <= i ? null : headings.get(i), i, inValuesByIndex.get(i));
				}
				
				return isContinue;
			}

			@Override
			public void handleDone(int inRowCount) throws Exception {

				inCellHandler.handleDone(inRowCount);
			}
		});

	}

	public void read(InputStream in, String inCharset, IRowHandler inRowHandler) throws Exception {

		if (in == null) {
			
			return;
		
		} else if ( inRowHandler == null ) {
			
			throw new IllegalArgumentException("IRowHandler must not be null");
		}

		final BufferedReader theReader = new BufferedReader(new InputStreamReader(in, inCharset == null ? Charset.defaultCharset() : Charset.forName(inCharset)));
		
		boolean checkFirstLine = isFirstLineHeadings;
		int theRowCount = 0;
		final LinkedHashMap<String, String> theCellValuesbyName = new LinkedHashMap<String, String>();
		final LinkedHashMap<Integer, String> theCellValuesbyIndex = new LinkedHashMap<Integer, String>();
		boolean isContinue = true;

		if (headings == null && checkFirstLine) {

			headings = new ArrayList<String>();
		}

		while (theReader.ready() && isContinue) {
			
			final String aLine = theReader.readLine();
			
			theCellValuesbyIndex.clear();
			theCellValuesbyName.clear();
			
			final String[] theCellValues = (!isTrimLines ? aLine : aLine.trim()).split(separatorPattern.pattern(), -1);
			
			for (int i = 0; i < theCellValues.length; i++) {
				
				String aCellValue = theCellValues[i];
				
				if (isCellWrapped) {

					aCellValue = aCellValue.trim();
					aCellValue = aCellValue.length() < 2 ? aCellValue : aCellValue.substring(1, aCellValue.length() - 1);
				}
				

				if (checkFirstLine && headings != null) {

					headings.add( aCellValue );
					
				} else {

					theCellValuesbyIndex.put(i, aCellValue);
					theCellValuesbyName.put(headings == null || i >= headings.size() ? String.valueOf(i) : headings.get(i), aCellValue);
				}
			}

			if (checkFirstLine) {
				
				checkFirstLine = false;
				continue;
			}
			
			checkFirstLine = false;

			if (theRowCount == 0 && !inRowHandler.begin(Collections.unmodifiableList(headings))) {

				isContinue = false;
				continue;
			}

			isContinue &= inRowHandler.handleRow(theRowCount++, theCellValuesbyIndex, theCellValuesbyName, headings == null ? null : theCellValuesbyIndex.size() - headings.size(), aLine);

			theRowCount++;
		}

		inRowHandler.handleDone(theRowCount);
		
	}

	public static void read(String inFile, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, IRowHandler inCellHandler) throws Exception {
		
		read(new File(inFile).toURI().toURL(), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inCellHandler);
	}

	public static void read(String inUrl, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, ICellHandler inCellHandler) throws Exception {

		read(new URL(inUrl), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inCellHandler);
	}

	public static void read(File inFile, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, ICellHandler inCellHandler) throws Exception {

		read(inFile == null ? null : inFile.toURI().toURL(), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inCellHandler);
	}

	public static void read(File inFile, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, IRowHandler inRowHandler) throws Exception {

		read(inFile == null ? null : inFile.toURI().toURL(), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inRowHandler);
	}

	public static void read(URL inUrl, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, ICellHandler inCellHandler) throws Exception {

		final InputStream theIs = inUrl.openStream();

		try {

			read(theIs, inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inCellHandler);

		} finally {

			Utils.close(theIs);
		}
	}

	public static void read(InputStream inIs, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, ICellHandler inCellHandler) throws Exception {

		new CsvReader(inSeparator, isCellWrapped, inFirstLineHeadings ).read(inIs, inCharset, inCellHandler);
	}

	public static void read(InputStream inIs, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, IRowHandler inRowHandler) throws Exception {

		new CsvReader(inSeparator, isCellWrapped, inFirstLineHeadings).read(inIs, inCharset, inRowHandler);
	}

	public static interface IRowHandler {

		boolean begin(List<String> inHeadings);

		boolean handleRow(int inRowIndex, Map<Integer, String> inValuesByIndex, Map<String, String> inValuesByName, Integer inHeadingDifference, String inLine) throws Exception;

		void handleDone(int inRowCount) throws Exception;
	}

	public static interface ICellHandler {

		boolean begin(List<String> inHeadings);

		boolean handleNewline(int inRowIndex, String inRow) throws Exception;

		boolean handleCell(int inRowIndex, String inColumnName, int inColumnIndex, String inValue) throws Exception;

		void handleDone(int inRowCount) throws Exception;
	}

	public static void readFile(String inFile, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, IRowHandler inRowHandler) throws Exception {

		read(new File(inFile).toURI().toURL(), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inRowHandler);
	}

	public static void readFile(String inFile, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, ICellHandler inCellHandler) throws Exception {

		read(new File(inFile).toURI().toURL(), inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inCellHandler);
	}

	public static void read(URL url, String inCharset, String inSeparator, boolean isCellWrapped, boolean inFirstLineHeadings, IRowHandler inRowHandler) throws Exception {

		final InputStream i = url.openStream();

		try {

			read(i, inCharset, inSeparator, isCellWrapped, inFirstLineHeadings, inRowHandler);

		} finally {

			Utils.close(i);
		}

	}
}
