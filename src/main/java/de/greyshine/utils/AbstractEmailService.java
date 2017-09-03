package de.greyshine.utils;


public abstract class AbstractEmailService implements IEmailService {

	@Override
	public void sendText(String inFromEmail, String inToEmail, String inSubject, String inText) throws SendException {

		send(new Email(inToEmail).from(inFromEmail).subject(inSubject).text(inText));
		
	}

	@Override
	public void sendHtml(String inToEmail, String inSubject, String inHtml, String inText) throws SendException {

		send(new Email(null, inToEmail).subject(inSubject).html(inHtml).text(inText));
	}

	@Override
	public void sendHtml(String inFromEmail, String inToEmail, String inSubject, String inHtml, String inText) throws SendException {

		send(new Email(null, inToEmail).from(inFromEmail).subject(inSubject).html(inHtml).text(inText));
	}

	@Override
	public void sendText(String inToEmail, String inSubject, String inText) throws SendException {

		send(new Email(inToEmail).subject(inSubject).text(inText));
	}

	@Override
	public void sendText(String inFromName, String inFromEmail, String inToName, String inToEmail, String inSubject, String inText) throws SendException {

		send(new Email(inFromName, inFromEmail, inToName, inToEmail).subject(inSubject).text(inText));
	}

	@Override
	public void sendHtml(String inFromName, String inFromEmail, String inToName, String inToEmail, String inSubject, String inHtml, String inText) throws SendException {

		send(new Email(inFromName, inFromEmail, inToName, inToEmail).subject(inSubject).text(inText).html(inHtml));
	}

}
