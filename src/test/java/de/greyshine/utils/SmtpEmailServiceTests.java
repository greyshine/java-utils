package de.greyshine.utils;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import de.greyshine.utils.Email.EType;
import de.greyshine.utils.IEmailService.SendException;

public class SmtpEmailServiceTests {
	
	static final Log LOG = LogFactory.getLog( SmtpEmailServiceTests.class );
	
	private static final SmtpEmailService s = new SmtpEmailService();
	
	public static boolean skip = false;
	
	@BeforeClass
	public static void beforeClass() throws IOException {
		
		final File pf = new File("src/test/resources/SmtpEmailServiceTests.properties"); 
		
		if ( !Utils.isFile(pf) ) {
			
			Utils.copy( new File("src/test/resources/SmtpEmailServiceTests.template.properties"), pf );
			skip = true;
		}
		
		Properties p = Utils.loadProperties( pf.getAbsolutePath() );

		s.setLogin(
				p.getProperty( "server.address" ),//
				p.getProperty( "server.port" ),//
				p.getProperty( "server.login" ),//
				p.getProperty( "server.password" ),//
				p.getProperty( "sender.name" ),//
				p.getProperty( "sender.email" ));
	}
	
	@Test
	@Ignore
	public void test() throws SendException {
		
		if ( skip ) { return; }
		
		String theHtml = "<html><body>Hallo Html-Email, das scheint gut zu funktionieren - auch programmatisch. Diese Email ist automatsich erstellt.<h1>" + Utils.formatDate( "dd.MM.yyyy HH:mm:ss" , LocalDateTime.now()) + "</h1><hr/><img src=\"cid:test\"/>" + "</body></html>";

		final Email e = Email.create("Hallo Wumpe", "kuemmel.dss@gmx.de", "java test betreff", null, "Some Plain Text");
		e.addAddress(EType.BCC, "Dirk Schumacher", "dirk.s.schumacher@web.de");
		e.inline("test", new File("src/test/resources/test.jpg"));
		e.attachment(new File("src/test/resources/test.png"));
		s.send(e);
		
	}

}
