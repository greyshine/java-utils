package de.greyshine.utils.deprecated;

public interface ILineHandler {

	boolean handle(int inLineNumber, String inLine) throws Exception;

	void done(int inLineCount) throws Exception;

}