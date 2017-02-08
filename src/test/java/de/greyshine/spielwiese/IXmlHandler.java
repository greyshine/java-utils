package de.greyshine.spielwiese;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;

public interface IXmlHandler {

	default void startDocument(XMLEvent inXmlEvent) {
	};

	default void endDocument(XMLEvent inXmlEvent) {
	};

	default void startElement(Element inElement, String inNamespace, String inElementName) {};

	default void endElement(Element inElement, String inNamespace, String inElementName) {};

	default void text(Element inElement, String inNamespace, String inElementName, String inText) {};

	default void attribute( Att inAttribute, String inElementNamespace, String inElementName, String inNamespace, String inAttributeName, String inAttributeValue) {};

	public class Att {
		
		public final Element element;
		public final String namespace; 
		public final String name;
		public final String path;
		public final String indexedPath;
		
		public Att(Element inElement, QName inQname) {
			
			element = inElement;
			
			namespace = inQname.getPrefix().isEmpty() ? null : inQname.getPrefix();
			name = inQname.getLocalPart();
					
			String theAttrPathPart = "/@";
			theAttrPathPart += (namespace == null ? "" : namespace + ":");
			theAttrPathPart += name;
			
			path = inElement.path + theAttrPathPart;
			indexedPath = inElement.indexedPath + theAttrPathPart;
		}
	}
	
	/**
	 * Helper class for tracking  
	 * 
	 *
	 */
	public class Element {

		public final XMLEvent xmlEvent;
		public final Element parent;
		private int childCount = 0;

		public final String name;
		public final String namespace;
		public final String path;
		public final String indexedPath;
		public final int childIndex;
		
		private final StringBuilder text = new StringBuilder();
		
		public Element(XMLEvent inXmlEvent, Element inParent, QName inName) {
			
			xmlEvent = inXmlEvent;
			parent = inParent;
			namespace = inName.getPrefix().isEmpty() ? null : inName.getPrefix();
			name = inName.getLocalPart();
			path = evalPath();
			
			if ( parent != null ) { parent.childCount++; }
			childIndex = parent == null ? 0 : parent.childCount-1;
			
			indexedPath = path+"["+childIndex+"]";
		}
		
		private String evalPath() {

			final List<Element> es = new ArrayList<>();

			Element e = this;

			while (e != null) {
				es.add(0, e);
				e = e.parent;
			}

			final StringBuilder s = new StringBuilder();

			es.forEach((anElement) -> {

				s.append('/');

				if (anElement.namespace != null) {
					s.append(anElement.namespace).append(':');
				}
				s.append(anElement.name);

			});

			return s.toString();
		}
		
		private void handleText(IXmlHandler inHandler) {

			if (text.length() == 0) {
				return;
			}
			
			inHandler.text(this, namespace, name, text.toString());
			text.setLength(0);
		}
		
		@Override
		public String toString() {
			return (namespace == null ? "" : namespace + ":") + name + " [path=" + path + "]";
		}
	}

	public static void execute(File inFile, String inEncoding, IXmlHandler inHandler) throws XMLStreamException, FileNotFoundException, IOException {
		
		if ( inFile == null || !inFile.isFile() ) { return; }
		
		execute( new FileInputStream( inFile ), inEncoding, inHandler);
	}
	
	public static void execute(String inXml, IXmlHandler inHandler) throws XMLStreamException {
		if ( inXml == null ) { return; }
		execute( new ByteArrayInputStream( inXml.getBytes( Charset.forName( "utf-8" ) ) ), "utf-8" , inHandler );
	}
	
	public static void execute(InputStream in, String inEncoding, IXmlHandler inHandler) throws XMLStreamException {
		
		final Charset c = Charset.forName( inEncoding == null ? "UTF-8" : inEncoding );
		execute( new InputStreamReader( in, c) , inHandler);
	}
	
	public static void execute(Reader in, IXmlHandler inHandler) throws XMLStreamException {

		if (in == null || inHandler == null) {
			return;
		}

		final XMLInputFactory theInputFactory = XMLInputFactory.newInstance();
		final XMLEventReader theEventReader = theInputFactory.createXMLEventReader(in);

		Element element = null;

		while (theEventReader.hasNext()) {

			final XMLEvent inXmlEvent = theEventReader.nextEvent();

			switch (inXmlEvent.getEventType()) {
			
			case XMLEvent.START_DOCUMENT:

				inHandler.startDocument(inXmlEvent);
				break;

			case XMLEvent.END_DOCUMENT:

				inHandler.endDocument(inXmlEvent);
				break;

			case XMLEvent.START_ELEMENT:

				if (element != null) {
					element.handleText(inHandler);
				}

				final StartElement se = inXmlEvent.asStartElement();
				element = new Element(inXmlEvent, element, se.getName());

				inHandler.startElement(element, element.namespace, element.name);

				@SuppressWarnings("unchecked")
				final Iterator<Attribute> iter = se.getAttributes();
				while (iter.hasNext()) {

					final Attribute a = iter.next();
					final Att theAtt = new Att( element, a.getName() );
					
					inHandler.attribute(theAtt, element.namespace, element.name, theAtt.namespace, theAtt.name, a.getValue());
				}

				break;
				
			case XMLEvent.END_ELEMENT:

				element.handleText(inHandler);

				inHandler.endElement(element, element.namespace, element.name);
				
				element = element.parent;

				break;

			case XMLEvent.CHARACTERS:

				final Characters characters = inXmlEvent.asCharacters();
				if (element != null) {
					element.text.append(characters.getData());
				}

				break;

			default:

				System.out.println("unhandled: " + inXmlEvent.getEventType() + " :: " + inXmlEvent);
				break;
			}
		}
	}
}
