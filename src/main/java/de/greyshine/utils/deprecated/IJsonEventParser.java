package de.greyshine.utils.deprecated;

import java.math.BigDecimal;

public interface IJsonEventParser {

	void documentStart();

	void documentEnd();

	void arrayStart(String inPath, String inSimplePath, String inParentName, Integer inParentIndex);

	void arrayEnd(String inPath, String inSimplePath, String inParentName, Integer inParentIndex);

	void objectStart(String inPath, String inSimplePath, String inParentName, Integer inParentIndex);

	void objectEnd(String inPath, String inSimplePath, String inParentName, Integer inParentIndex);

	void handleNull(String inPath, String inSimplePath, String inName, Integer inIndex);

	void handle(String inPath, String inSimplePath, String inName, Integer inIndex, String inText);

	void handle(String inPath, String inSimplePath, String inName, Integer inIndex, boolean inBoolean);

	void handle(String inPath, String inSimplePath, String inName, Integer inIndex, BigDecimal inNumber);

	public static class Parser implements IJsonEventParser {

		private boolean defaultSystemOut = false;

		@Override
		public void documentStart() {
			if (defaultSystemOut) {
				System.out.println("START");
			}
		}

		@Override
		public void documentEnd() {
			if (defaultSystemOut) {
				System.out.println("END");
			}
		}

		@Override
		public void arrayStart(String inPath, String inSimplePath, String inParentName, Integer inParentIndex) {

			if (defaultSystemOut) {
				System.out.println("[]> " + inPath + " [name=" + inParentName + ", index=" + inParentIndex + "]");
			}

		}

		@Override
		public void arrayEnd(String inPath, String inSimplePath, String inParentName, Integer inParentIndex) {
			if (defaultSystemOut) {
				System.out.println("<[] " + inPath + " [name=" + inParentName + ", index=" + inParentIndex + "]");
			}
		}

		@Override
		public void objectStart(String inPath, String inSimplePath, String inParentName, Integer inParentIndex) {
			if (defaultSystemOut) {
				System.out.println("{}> " + inPath + " [name=" + inParentName + ", index=" + inParentIndex + "]");
			}
		}

		@Override
		public void objectEnd(String inPath, String inSimplePath, String inParentName, Integer inParentIndex) {
			if (defaultSystemOut) {
				System.out.println("<{} " + inPath + " [name=" + inParentName + ", index=" + inParentIndex + "]");
			}
		}

		@Override
		public void handleNull(String inPath, String inSimplePath, String inName, Integer inIndex) {
			if (defaultSystemOut) {
				System.out.println("value NULL " + inPath + " [name=" + inName + ", index=" + inIndex + "]");
			}
		}

		@Override
		public void handle(String inPath, String inSimplePath, String inName, Integer inIndex, String inText) {
			if (defaultSystemOut) {
				System.out.println("value TEXT " + inPath + " [name=" + inName + ", index=" + inIndex + "]: " + inText);
			}
		}

		@Override
		public void handle(String inPath, String inSimplePath, String inName, Integer inIndex, boolean inBoolean) {
			if (defaultSystemOut) {
				System.out.println("value BOOLEAN " + inPath + " [name=" + inName + ", index=" + inIndex + "]: " + inBoolean);
			}
		}

		@Override
		public void handle(String inPath, String inSimplePath, String inName, Integer inIndex, BigDecimal inNumber) {
			if (defaultSystemOut) {
				System.out.println("value NUMBER " + inPath + " [name=" + inName + ", index=" + inIndex + "]: " + inNumber.toPlainString());
			}
		}

		public IJsonEventParser defaultSystemOut(boolean b) {
			defaultSystemOut = b;
			return this;
		}
	}

}