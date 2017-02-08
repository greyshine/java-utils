package de.greyshine.utils;

import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.XMLEvent;

import org.junit.Assert;
import org.junit.Test;

public class IXmlHandlerTests {
	
	@Test
	public void test() throws XMLStreamException {
		
		final StringBuilder sb = new StringBuilder();
		
		IXmlHandler theXmlHandler = new IXmlHandler() {

			@Override
			public void startDocument(XMLEvent inXmlEvent) {
				sb.append( "START" );
			}

			@Override
			public void endDocument(XMLEvent inXmlEvent) {
				sb.append( "END" );
			}

			@Override
			public void startElement(Element inElement, String inNamespace, String inElementName) {
				Assert.assertNotNull( inElement );
				Assert.assertTrue( Utils.isNotBlank( inElementName ) );
				sb.append("SE"+inNamespace+inElementName );
			}

			@Override
			public void endElement(Element inElement, String inNamespace, String inElementName) {
				
				Assert.assertNotNull( inElement );
				Assert.assertTrue( Utils.isNotBlank( inElementName ) );
				sb.append("EE:"+inNamespace+inElementName );
				
			}

			@Override
			public void text(Element inElement, String inNamespace, String inElementName, String inText) {
				sb.append(inText);
			}

			@Override
			public void attribute(Att inAttribute, String inElementNamespace, String inElementName, String inNamespace,
					String inAttributeName, String inAttributeValue) {
				
				Assert.assertNotNull( inAttribute );
				Assert.assertTrue( Utils.isNotBlank( inElementName ) );
				Assert.assertTrue( Utils.isNotBlank( inAttributeName ) );
				sb.append("ATT:"+inNamespace+inAttributeName+inAttributeValue );
			}
		};
		
		IXmlHandler.execute( Utils.getResource( "src/test/resources/xmltesting/test1.xml" ) , "UTF-8", theXmlHandler);
		
		Assert.assertEquals( 279 , sb.toString().replaceAll( "\\s" , "").length() );
		Assert.assertEquals( "afd7aeda7ad832f74aa1f33bba61f3f2a13de37c3d33a3724a9e6a4f02e448ff" , Utils.getSha256( sb.toString() ) );
	}

}
