package de.greyshine.utils.deprecated;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class DefaultErrorHandler implements ErrorHandler {

	Log LOG = LogFactory.getLog(DefaultErrorHandler.class);

	@Override
	public void warning(SAXParseException exception) throws SAXException {

		LOG.warn(exception, exception);
	}

	@Override
	public void error(SAXParseException exception) throws SAXException {

		throw exception;
	}

	@Override
	public void fatalError(SAXParseException exception) throws SAXException {
		throw exception;
	}

}
