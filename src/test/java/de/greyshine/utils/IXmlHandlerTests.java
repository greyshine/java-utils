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
		
		// local environment and travis-ci treat the strings differently
		

		String theExpected =     "STARTSEnullmainSEnullsub1SEnullsub2ATT:nullatt1att1val1EE:nullsub2EE:nullsub1SEothersub2SomeTextEE:othersub2SEnullsub2ATT:otherattkeysomevalueSomeSEnullimoreEE:nulliTextSEnullbrEE:nullbrEE:nullsub2SEnullsub3ATT:otherattkeysomevalueLineoneLinetwoLineThreeEE:nullsub3EE:nullmainEND";
		String theTravisResult = "STARTSEnullmainSEnullsub1SEnullsub2ATT:nullatt1att1val1EE:nullsub2EE:nullsub1SEothersub2SomeTextEE:othersub2SEnullsub2ATT:otherattkeysomevalueSomeSEnullimoreEE:nulliTextSEnullbrEE:nullbrEE:nullsub2SEnullsub3ATT:otherattkeysomevalueEE:nullsub3EE:nullmainEND";
		String theResult = sb.toString().replaceAll( "\\s" , "");
		
		System.out.println( "expected: "+ theExpected );
		System.out.println( "result:   "+ theResult );
		
		final boolean isEqual = theExpected.equals( theResult );
		
		System.out.println( "equal: "+ isEqual );
		
		// TODO: travis-ci fails, it seems that the CDATA and its new lines are handled differently on different environments.
		System.err.println( "on travis-ci this fails. TODO: check why." );

		Assert.assertTrue( isEqual);
	}

}
