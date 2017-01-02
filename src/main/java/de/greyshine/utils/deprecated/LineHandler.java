package de.greyshine.utils.deprecated;

import java.util.HashSet;
import java.util.Set;


public abstract class LineHandler implements ILineHandler {

	final boolean trimLines;
	final boolean ignoreBlankLines;
	final String splitRegex;
	final Set<String> lineCommmentStarts;

	public LineHandler(boolean inTrimLines, boolean inIgnoreBlankLines, String inSplitRegex, String... inCommentLineStarts) {

		trimLines = inTrimLines;
		ignoreBlankLines = inIgnoreBlankLines;
		splitRegex = inSplitRegex;

		final Set<String> theCommentLineStarts = new HashSet<String>();
		for (final String aCommentLineStart : inCommentLineStarts) {

			if (!Utils.isBlank(aCommentLineStart)) {

				theCommentLineStarts.add(aCommentLineStart);
			}
		}

		lineCommmentStarts = theCommentLineStarts;
	}

	@Override
	public boolean handle(int inLineNumber, String inLine) throws Exception {

		inLine = !trimLines ? inLine : inLine.trim();

		if (ignoreBlankLines && Utils.isBlank(inLine)) {
			return true;
		}

		for (final String aCommentLineStart : lineCommmentStarts) {
			if (inLine.startsWith(aCommentLineStart)) {
				return true;
			}
		}

		final String[] theSplitParts = splitRegex == null ? new String[] { inLine } : inLine.split(splitRegex, -1);

		handleLine(inLineNumber, inLine, theSplitParts);

		return true;
	}

	public abstract void handleLine(int inLineNumber, String inLine, String[] inSplitParts) throws Exception;
}