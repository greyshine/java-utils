package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.mail.Address;
import javax.mail.Authenticator;
import javax.mail.Message;
import javax.mail.Message.RecipientType;
import javax.mail.MessagingException;
import javax.mail.Multipart;
import javax.mail.PasswordAuthentication;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

import de.greyshine.utils.Email.BytesAttachment;
import de.greyshine.utils.Email.FileAttachment;

/**
 * http://www.tutorialspoint.com/java/java_sending_email.htm<br/>
 * http://codereview.stackexchange.com/questions/12529/sending-html-formatted-
 * mail-in-java<br/>
 * http://stackoverflow.com/questions/322298/how-to-send-html-email-to-outlook-
 * from-java<br/>
 * http://www.mkyong.com/java/javamail-api-sending-email-via-gmail-smtp-example/<br/>
 * http://www.tutorialspoint.com/javamail_api/
 * javamail_api_send_inlineimage_in_email.htm<br/>
 * 
 * @author greyshine
 */
public class SmtpEmailService extends AbstractEmailService {

	public static final String PROPERTY_SERVER_ADDRESS = "server.address";
	public static final String PROPERTY_SERVER_PORT = "server.port";
	public static final String PROPERTY_SERVER_LOGIN = "server.login";
	public static final String PROPERTY_SERVER_PASSWORD = "server.password";
	/**
	 * set with <code>true</code> or anything else for <code>false</code>
	 */
	public static final String PROPERTY_SERVER_STARTTLS = "server.starttls";
	public static final String PROPERTY_SENDER_NAME = "sender.name";
	public static final String PROPERTY_SENDER_EMAIL = "sender.email";
	
	private static final String KEY_FROM_NAME = "mail.smtp.from.name";
	private static final String KEY_FROM_EMAIL = "mail.smtp.from.email";
	private static final String KEY_HOST = "mail.smtp.host";
	private static final String KEY_PORT = "mail.smtp.port";
	private static final String KEY_USER = "mail.smtp.user";
	private static final String KEY_PASSWORD = "mail.smtp.password";
	/**
	 * Takes <code>true</code> or <code>false</code>
	 */
	public final String KEY_AUTH = "mail.smtp.auth";
	/**
	 * Takes <code>true</code> or <code>false</code>
	 */
	public final String KEY_STARTTLS_ENABLE = "mail.smtp.starttls.enable";

	private Properties properties = new Properties();

	public SmtpEmailService() {
		this(true);
	}
	
	public SmtpEmailService(boolean inEnableStarttls) {
		if ( inEnableStarttls ) {
			enableStarttls();
		}
	}
	
	public SmtpEmailService( Properties inProperties ) {
		
		init( inProperties );
	}
	
	public SmtpEmailService init(Properties inProperties) {
		
		inProperties = inProperties != null ? inProperties : createProperties(null,null,null,null,null,null,null);
		
		setLogin( 
				Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SERVER_ADDRESS ) ),//
				Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SERVER_PORT ) ),
				Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SERVER_LOGIN ) ) ,//
				Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SERVER_PASSWORD ) )//
				);
		
		setFrom( Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SENDER_NAME ) ),//
				Utils.trimToEmpty( inProperties.getProperty( PROPERTY_SENDER_EMAIL ) )//
				);
		
		if ( "true".equalsIgnoreCase( inProperties.getProperty( PROPERTY_SERVER_STARTTLS, "false") ) ) {
			enableStarttls();
		}
		
		return this;
	}

	public static Properties createProperties(String serverAddress, String serverPort, String serverLogin, String serverPassword, String inUseStarttls, String senderName, String senderEmail) {
		
		final Properties p = new Properties();
		
		p.setProperty( PROPERTY_SERVER_ADDRESS ,  Utils.trimToEmpty( serverAddress ) );
		p.setProperty( PROPERTY_SERVER_LOGIN ,  Utils.trimToEmpty( serverLogin ) );
		p.setProperty( PROPERTY_SERVER_PASSWORD ,  Utils.trimToEmpty( serverPassword ) );
		p.setProperty( PROPERTY_SERVER_STARTTLS ,  "true".equalsIgnoreCase( inUseStarttls ) ? "true" : "false" );
		p.setProperty( PROPERTY_SERVER_PORT ,  String.valueOf( Utils.parseInteger( serverPort , -1) ) );
		
		p.setProperty( PROPERTY_SENDER_NAME ,  Utils.trimToEmpty( senderName ) );
		p.setProperty( PROPERTY_SENDER_EMAIL ,  Utils.trimToEmpty( senderEmail ) );
		
		return p;
	}

	public void setProperties(Properties inProperties) {
		if (inProperties != null) {
			properties.putAll(inProperties);
		}
	}

	public void setHost(String inHost, int inPort) {
		setHost(inHost, String.valueOf(inPort));
	}
	public void setHost(String inHost, String inPort) {

		setProperty(KEY_HOST, inHost);
		setProperty(KEY_PORT, inPort);
	}

	/**
	 * @param inHost
	 * @param inPort
	 * @param inUser
	 * @param inPassword
	 * @param inFromName
	 * @param inFromEmail
	 */
	public void setLogin(String inHost, String inPort, String inUser, String inPassword, String inFromName, String inFromEmail) {

		setLogin(inHost, inPort, inUser, inPassword);
		setFrom(inFromName, inFromEmail);
	}

	public void setFrom(String inFromName, String inFromEmail) {
		setProperty(KEY_FROM_NAME, Utils.defaultIfBlank(inFromName, ""));
		setProperty(KEY_FROM_EMAIL, inFromEmail);
	}

	public void setLogin(String inHost, String inPort, String inUser, String inPassword) {

		setHost(inHost, inPort);
		setLogin(inUser, inPassword);
	}

	public void setLogin(String inUser, String inPassword) {

		setProperty(KEY_AUTH, "true");
		setProperty(KEY_USER, inUser);
		setProperty(KEY_PASSWORD, inPassword);
	}

	public SmtpEmailService enableStarttls() {

		setProperty(KEY_STARTTLS_ENABLE, "true");
		return this;
	}

	public void setProperty(String inKey, String inValue) {

		switch ( inKey ) {
		case PROPERTY_SERVER_ADDRESS:
			inKey = KEY_HOST;
			break;
		case PROPERTY_SERVER_LOGIN:
			inKey = KEY_USER;
			break;
		case PROPERTY_SERVER_PASSWORD:
			inKey = KEY_PASSWORD;
			break;
		case PROPERTY_SENDER_EMAIL:
			inKey = KEY_FROM_EMAIL;
			break;
		case PROPERTY_SENDER_NAME:
			inKey = KEY_FROM_NAME;
			break;
		}
		
		properties.put(inKey, inValue);
	}

	@Override
	public void send(Email inEmail) throws SendException {

		if (inEmail == null) {

			return;
		}
		
		try {
			
			Authenticator theAuthenticator = null;

			if (properties.get(KEY_AUTH) != null && "true".equalsIgnoreCase(properties.get(KEY_AUTH).toString())) {

				theAuthenticator = new Authenticator() {

					@Override
					protected PasswordAuthentication getPasswordAuthentication() {
						return new PasswordAuthentication(properties.getProperty(KEY_USER), properties.getProperty(KEY_PASSWORD));
					}
				};
			}

			final Session theSession = Session.getDefaultInstance(properties, theAuthenticator);
			final Message theMessage = createJavaxMessage(theSession, inEmail);
			Transport.send(theMessage);
			inEmail.markSend();

		} catch (final Exception e) {

			inEmail.markSend(e);
			
			throw e instanceof SendException ? (SendException) e : new SendException(e);
		}
	}
	
	protected Message createJavaxMessage(Session inSession, Email inEmail) throws MessagingException, IOException {
		
		final MimeMessage theMessage = new MimeMessage(inSession);
		
		final List<InternetAddress> theFroms = new ArrayList<InternetAddress>(1);
		
		for (final Email.EmailAddress ea : inEmail.emails) {

			final InternetAddress anAddress = new InternetAddress(ea.email, Utils.trimToNull(ea.name));
			
			switch (ea.type) {

			case FROM:
				theFroms.add(anAddress);
				break;
			case TO:
				theMessage.addRecipient(RecipientType.TO, anAddress);
				break;
			case CC:
				theMessage.addRecipient(RecipientType.CC, anAddress);
				break;
			case BCC:
				theMessage.addRecipient(RecipientType.BCC, anAddress);
				break;
			default:
				throw new MessagingException("Type of EmailAdress unknown: " + ea.type);
			}
		}

		if (theFroms.isEmpty() && properties.getProperty(KEY_FROM_EMAIL) != null) {

			theMessage.addFrom(new Address[] { new InternetAddress(properties.getProperty(KEY_FROM_EMAIL), properties.getProperty(KEY_FROM_NAME), "UTF-8") });

		} else {

			theMessage.addFrom(theFroms.toArray(new InternetAddress[theFroms.size()]));
		}
		
		theMessage.setSubject(Utils.trimToEmpty(inEmail.subject).replaceAll("\n\r", " "));

		boolean isHtml = false;
		Multipart theMultipart = null;
		
		if (inEmail.isHtml()) {
			
			final String theContentType = "text/html" + ( inEmail.getCharset() == null ? "" : "; charset="+ inEmail.getCharset().name() );
			
			isHtml = true;
			theMultipart = theMultipart != null ? theMultipart : createMultipart(isHtml, inEmail.isText() ? inEmail.getText() : null);
			
			final MimeBodyPart theMbp = new MimeBodyPart();
			final String theHtml = Utils.trimToEmpty( inEmail.getHtml() );
			theMbp.setContent( theHtml , theContentType);
			theMultipart.addBodyPart( theMbp );
			theMessage.setContent(Utils.trimToEmpty(inEmail.getHtml()), theContentType);
		}

		if (inEmail.isText() ) {
			
			theMessage.setText(Utils.trimToEmpty(inEmail.getText()), ( inEmail.getCharset() != null ? inEmail.getCharset() : Charset.defaultCharset() ).name() );
		}

		for (
		final Email.Attachment a : inEmail.attachments) {

			theMultipart = theMultipart != null ? theMultipart : createMultipart(isHtml, inEmail.isText() ? inEmail.getText() : null);
			
			final MimeBodyPart theMbp = new MimeBodyPart();

			if (a instanceof FileAttachment) {

				theMbp.attachFile(((FileAttachment) a).file);
				theMbp.setFileName(a.name);

			} else if (a instanceof BytesAttachment) {

				theMbp.setFileName(a.name);
				theMbp.setDataHandler(new DataHandler( new DataSource() {
					
					@Override
					public OutputStream getOutputStream() throws IOException {
						return Utils.DEV0;
					}
					
					@Override
					public String getName() {
						return a.name;
					}
					
					@Override
					public InputStream getInputStream() throws IOException {
						return new ByteArrayInputStream(((BytesAttachment) a).bytes);
					}
					
					@Override
					public String getContentType() {
						return a.contentType;
					}
				}  ));
			
			} else {
				
				throw new IOException("Unknown handling of attachment/inline element: " + a);
			}

			theMbp.setFileName(a.name);

			if (!a.isInline) {

				theMbp.setDisposition(MimeBodyPart.ATTACHMENT);

			} else {

				theMbp.setDisposition(MimeBodyPart.INLINE);
				theMbp.setHeader("Content-ID", "<" + a.name + ">");
			}

			theMultipart.addBodyPart(theMbp);
		}
		
		if ( theMultipart != null ) {
			
			theMessage.setContent( theMultipart );
		}

		return theMessage;
	}

	private Multipart createMultipart(boolean inIsAlternativeText, String inText) throws MessagingException {

		final MimeMultipart theMimeMultipart = new MimeMultipart("related");

		if (inText != null && !inIsAlternativeText) {

			final MimeBodyPart theBodyPart = new MimeBodyPart();
			theBodyPart.setContent(inText, "text/plain");
			theMimeMultipart.addBodyPart(theBodyPart);
		}

		return theMimeMultipart;
	}

}

