package de.greyshine.utils.beta.businessinfos;

/**
 * Interface uniquely defining an error.
 * Num values larger than 300 are considered to errornous
 */
public interface IStatusCode {
	
	enum Code implements IStatusCode {
		
		UNKNOWN(0),
		OK(200),
		ERROR_UNKNOWN(500);

		public final int num;
		
		private Code(int num) {
			this.num = num;
		}

		public int num() {
			return num;
		}
	}
	
	int num();
	String name();
	default String getGuiKey() {
		return name();
	}
	default boolean isError() {
		switch ( String.valueOf( Math.abs( num() ) ).charAt(0) ) {
		case '0':
		case '1':
		case '2':
		case '3':
			return false;
		default:
			return true;
		}
	}
}
