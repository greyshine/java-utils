package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.List;


public class CollectLinesHandler extends LineHandler {

	final List<String> lines;

	public CollectLinesHandler() {

		this(null);
	}

	public CollectLinesHandler(List<String> inLines) {

		this(inLines, false, false, (String[]) null);
	}

	@Override
	public void done(int inLineCount) throws Exception {
	}

	public CollectLinesHandler(List<String> inLines, boolean inTrimLines, boolean inIgnoreBlankLines, String... inCommentLineStarts) {

		super(inTrimLines, inIgnoreBlankLines, null, inCommentLineStarts);
		lines = inLines != null ? inLines : new ArrayList<String>();
	}

	@Override
	public void handleLine(int inLineNumber, String inLine, String[] inSplitParts) throws Exception {

		lines.add(inLine);
	}

	public List<String> getLines() {
		return lines;
	}

}