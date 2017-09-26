package de.greyshine.utils;

public interface IEmailService {

	/**
	 * Will use the services default <tt>from</tt> if set.
	 * 
	 * @param inToEmail
	 * @param inSubject
	 * @param inText
	 * @throws SendException
	 */
	void sendText(String inToEmail, String inSubject, String inText) throws SendException;
	void sendText(String inFromEmail, String inToEmail, String inSubject, String inText) throws SendException;
	void sendText(String inFromName, String inFromEmail, String inToName, String inToEmail, String inSubject, String inText) throws SendException;

	/**
	 * Will use the services default <tt>from</tt> if set.
	 * 
	 * @param inToEmail
	 * @param inSubject
	 * @param inHtml
	 * @throws SendException
	 */
	void sendHtml(String inToEmail, String inSubject, String inHtml, String inText) throws SendException;
	void sendHtml(String inFromEmail, String inToEmail, String inSubject, String inHtml, String inText) throws SendException;
	void sendHtml(String inFromName, String inFromEmail, String inToName, String inToEmail, String inSubject, String inHtml, String inText) throws SendException;

	void send(Email inEmail) throws SendException;
	void send(Email inEmail, long inMaxTimeOut) throws SendException;
	
	class SendException extends Exception {

		private static final long serialVersionUID = 4238210987914193737L;

		public SendException(Exception inException) {
			this(null, inException);
		}

		public SendException(String inMessage) {
			this(inMessage, null);
		}

		public SendException(String inMessage, Exception inRootCause) {
			super(Utils.defaultIfBlank(inMessage, inRootCause == null ? "unknown reason" : Utils.toString(inRootCause)), inRootCause);
		}
	}


}
