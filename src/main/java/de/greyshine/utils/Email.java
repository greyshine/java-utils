package de.greyshine.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

public class Email implements Serializable {

	private static final long serialVersionUID = -5649175876793611233L;

	public enum EType {

		TO, CC, BC, BCC, FROM;

		public boolean isReceiver() {

			return this != FROM;
		}

		public boolean isSender() {

			return !isReceiver();
		}
	}
	
	final List<EmailAddress> emails = new ArrayList<EmailAddress>(2);
	final List<Attachment> attachments = new ArrayList<Attachment>(0);
	private StringBuilder text = null;
	private StringBuilder html = null;
	String subject = "";
	private Long sendtime = null;
	private Exception sendException = null;
	private Charset charset = null;
	
	public Email(String inFromName, String inFromEmail, String inToName, String inToEmail) {
	
		if (inFromEmail != null) {

			addAddress(EType.FROM, inFromName, inFromEmail);
		}

		addAddress(EType.TO, inToName, inToEmail);
	}

	public Email(String inToName, String inToEmail) {

		this(null, null, inToName, inToEmail);
	}

	public Email(String inToEmail) {

		this(null, null, null, inToEmail);
	}

	public Email() {
	}

	public Email charset( Charset inCharset ) {
		charset = inCharset;
		return this;
	}
	
	public Email addRecipient(String inEmail) {

		return addAddress(EType.TO, null, inEmail);
	}

	public Email addFrom(String inEmail) {

		return addAddress(EType.TO, null, inEmail);
	}

	public Email addAddress(EType inType, String inName, String inEmail) {

		if (inType == null) {
			throw new IllegalArgumentException("Type must be not null.");
		}
		
		if (inEmail != null) {

			emails.add(new EmailAddress(inType, inName, inEmail));
		}

		return this;
	}
	
	public Email addText(String inText) {
		
		text = (text == null ? new StringBuilder() : text).append(inText == null ? "" : inText);
		return this;
	}
	
	public Email addTextLine(String inText) {
		
		return addText( (inText==null?"":inText) +"\n" );
	}

	public String getText() {
		
		return text == null ? "" : text.toString();
	}

	public String getHtml() {
		return html == null ? "" : html.toString();
	}
	
	public Charset getCharset() {
		return charset;
	}

	public boolean isHtml() {
		return html != null && html.length() > 0;
	}

	public boolean isText() {
		
		boolean isText = text != null && text.length() > 0;
		
		// if it is not text and not html, make it text, otherwise exception is thrown:
		// java.io.IOException: No content; at com.sun.mail.smtp.SMTPTransport.sendMessage(SMTPTransport.java:625)
		if ( !isText && !isHtml() ) {
			return true;
		}
		
		return isText;
	}

	public static class EmailAddress {

		public final EType type;
		public final String name;
		public final String email;

		private EmailAddress(EType inType, String name, String email) {

			type = inType;
			this.name = Utils.trimToNull(name);
			this.email = email;
		}
	}

	public Email subject(String inSubject) {
		
		this.subject = inSubject == null ? "" : inSubject.trim();
		
		return this;
	}

	public Email text(String inText) {

		this.text = new StringBuilder().append(inText == null ? "" : inText);
		return this;
	}
	
	public Email html(String inHtml) {

		this.html = new StringBuilder().append(inHtml == null ? "" : inHtml);
		return this;
	}

	public Email attachment(File inFile) {

		return attachment(inFile == null ? null : inFile.getName(), inFile);
	}

	public Email attachment(String inName, File inFile) {

		if (inFile == null || !inFile.isFile()) {

			return this;
		}
		
		attachments.add(new FileAttachment(false, inName, inFile));

		return this;
	}

	public Email attachment(String inName, InputStream inStream, String inContentType) throws IOException {

		if (inStream == null) {

			return this;
		}

		attachments.add(new InputStreamAttachment(false, inName == null ? String.valueOf(attachments.size() + 1) : inName, inStream, inContentType));

		return this;
	}

	public Email inline(String inName, File inFile) {

		if (inFile == null || !inFile.isFile()) {

			return this;
		}

		attachments.add(new FileAttachment(true, inName, inFile));

		return this;
	}

	public Email inline(String inName, InputStream inData) throws IOException {

		if (inData == null) {

			return this;
		}

		attachments.add(new InputStreamAttachment(true, inName == null ? String.valueOf(attachments.size() + 1) : inName, inData, null));

		return this;
	}

	void markSend() {

		this.markSend(null);
	}

	void markSend(Exception inException) {

		sendtime = System.currentTimeMillis();
		sendException = inException;
	}

	public Long getSendTime() {

		return sendtime;
	}
	
	public boolean isException() {

		return sendException != null;
	}

	public Exception getException() {

		return sendException;
	}

	static abstract class Attachment {

		final String name;
		final boolean isInline;
		final String contentType;

		private Attachment(boolean isInline, String name, String inContentType) {

			this.name = name;
			this.isInline = isInline;
			this.contentType = Utils.defaultIfBlank(inContentType, null);
		}
	}

	static class FileAttachment extends Attachment {

		final File file;

		private FileAttachment(boolean isInline, String inName, File inFile) {

			super(isInline, Utils.defaultIfBlank(inName, inFile.getName()), null);
			file = inFile;
		}
	}

	static class URLAttachment extends Attachment {

		final URL url;

		private URLAttachment(boolean isInline, String inName, URL inUrl, String inContentType) {

			super(isInline, Utils.defaultIfBlank(inName, null), null);
			url = inUrl;
		}
	}

	static class BytesAttachment extends Attachment {

		final byte[] bytes;

		private BytesAttachment(boolean isInline, String inName, byte[] inBytes, String inContentType) {

			super(isInline, Utils.defaultIfBlank(inName, null), inContentType);
			bytes = inBytes;
		}
	}

	static class InputStreamAttachment extends BytesAttachment {

		private InputStreamAttachment(boolean isInline, String inName, InputStream inInputStream, String inContentType) throws IOException {

			super(isInline, inName, Utils.toBytes(inInputStream), inContentType);
		}
	}

	public static Email create(String inToName, String inToEmail, String inSubject, String inHtml, String inText) {

		return new Email(inToName, inToEmail).subject(inSubject).html(inHtml).text(inText);
	}

	public Email from(String inFromEmail) {
		
		return from(null, inFromEmail);
	}

	public Email from(String inFromName, String inFromEmail) {

		return addAddress(EType.FROM, inFromName, inFromEmail);
	}

}
