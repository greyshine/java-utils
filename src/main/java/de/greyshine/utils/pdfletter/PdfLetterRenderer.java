package de.greyshine.utils.pdfletter;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.net.URL;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Consumer;

import org.apache.commons.cli.UnrecognizedOptionException;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.TemplateSpec;
import org.thymeleaf.context.Context;
import org.thymeleaf.templatemode.TemplateMode;
import org.w3c.dom.Document;
import org.xhtmlrenderer.layout.SharedContext;
import org.xhtmlrenderer.pdf.ITextRenderer;
import org.xhtmlrenderer.pdf.ITextUserAgent;
import org.xhtmlrenderer.resource.XMLResource;
import org.xml.sax.InputSource;

import de.greyshine.utils.FieldHandler;
import de.greyshine.utils.MethodHandler;
import de.greyshine.utils.ReflectionUtils;
import de.greyshine.utils.Utils;
import de.greyshine.utils.Utils.OutputInputStreams;

/**
 * Reuse an {@link PdfLetterRenderer} instance in order to speed up the actual render process.<br/>
 * <br/>
 * The generic type &lt;T&gt; is determining the type of the data object.<br/>
 * The data object is a Class for holding variables.<br/>
 * It must contain a default empty public constructor in order to be automatically instantiated.<br/>
 * All public fields and methods are used as variables by their name.<br/>
 * If a member or method is annotated with @{@link Variable} it is used as well as a variable and if the value is not blank, it is used as well as a variable.<br/>
 * <br/>
 * In order to add functionalities like initialization on a data object, let it implement the IDataObject interface.<br/>
 * <br/>
 *
 * @param <T> Type of the data object
 */
public class PdfLetterRenderer<T extends PdfLetterRenderer.DataObject> {
	
	public static final String SPACE_ENTITY = "&#160;";
	public static final String SPACE_ENTITY_TAB = "&#160;&#160;&#160;&#160;";
	
	private static final TemplateEngine THYMELEAF_TEMPLATEENGINE = new TemplateEngine();

	private final ITemplate<?> templateInformation;
	
	private String xhtmlDocument;
	
	private Map<String,Field> dataFields = new HashMap<>(0);
	private Map<String,Method> dataMethods = new HashMap<>(0);
	
	private final Class<? extends DataObject> dataObjectClass;
	private final String uriBasepath;
	
	private boolean allowInternetUrlStreaming = false;
	
	public PdfLetterRenderer(ITemplate<T> inTemplateInformation) throws IOException {
		
		Utils.requireNotNull( inTemplateInformation, "given ITemplate is null" );
		
		templateInformation = inTemplateInformation;
		
		final InputStream theXhtmlInputStream = inTemplateInformation.getHtml().openStream(); 
		
		try {
		
			xhtmlDocument = Utils.readToString( theXhtmlInputStream, Utils.CHARSET_UTF8);
			XMLResource.load(new InputSource( new ByteArrayInputStream( xhtmlDocument.getBytes( Utils.CHARSET_UTF8 ) ) )).getDocument();
			
		} finally {

			Utils.close( theXhtmlInputStream );
		}
		
		dataObjectClass = inTemplateInformation.getDataObjectClass();
		
		if ( dataObjectClass == null || !ReflectionUtils.isDefaultConstructorAvailable( dataObjectClass ) ) {
			throw new IllegalArgumentException( "data-object class is not declaring a default constructor at "+ inTemplateInformation );
		}
		
		uriBasepath = dataObjectClass.getPackage().getName().replace( '.' , '/')+"/";
		
		newDataObject();
		
		if ( dataObjectClass != null ) {
			
			ReflectionUtils.traversFieldHierarchy( dataObjectClass, new FieldHandler() {
				
				{
					super.handlePublic = true;
				}

				@Override
				public boolean doHandle(Field inField) {
					
					final Variable theVariable = inField.getDeclaredAnnotation( Variable.class );
					
					if ( theVariable == null && !ReflectionUtils.isPublic( inField ) ) {
						return true;
					}
					
					final String theName = Utils.defaultIfBlank( theVariable == null ? null : theVariable.value(), inField.getName() );
					
					dataFields.put( theName , inField);
					
					return true;
				}
			});	

			ReflectionUtils.traversMethodHierarchy( dataObjectClass, new MethodHandler() {

				{
					super.handlePublic = true;
					super.skipObject = true;
					super.skipReturnVoid = true;
					super.methodArguments = Utils.EMPTY_CLASSES;
				}
				
				@Override
				public boolean doHandle(Method inMethod) {
					
					final Variable theVariable = inMethod.getDeclaredAnnotation( Variable.class );
					
					if ( theVariable == null && !ReflectionUtils.isPublic( inMethod ) ) {
						return true;
					}
					
					final String theName = Utils.defaultIfBlank( theVariable == null ? null : theVariable.value(), inMethod.getName() );
					
					dataMethods.put( theName , inMethod);
					
					//System.out.println( inMethod.getReturnType() +" "+ inMethod +" "+ inMethod.getDeclaringClass() );
					
					return true;
				}
			} );
		}
	}
	
	public PdfLetterRenderer<T> allowInternetUrlStreaming(boolean inAllowInternetUrlStreaming) {
		allowInternetUrlStreaming = inAllowInternetUrlStreaming;
		return this;
	}
	
	/**
	 * @return <code>true</code> in order to allow URI lookups from the HTML into public internet
	 */
	public boolean isAllowInternetUrlStreaming() {
		return allowInternetUrlStreaming;
	}

	public void setAllowInternetUrlStreaming(boolean allowInternetUrlStreaming) {
		this.allowInternetUrlStreaming = allowInternetUrlStreaming;
	}
	
	public void render(T inData, OutputStream inOs) throws Exception {
		
		if ( inOs == null ) {
			throw new IllegalArgumentException( "OutputStream is null" );
		}
		
		final Map<String,Object> theVariables = dataToVariables(inData);
		
		
		
		
		final InputStream theXhtmlIs = runThymeleaf( new ByteArrayInputStream( xhtmlDocument.getBytes( Utils.CHARSET_UTF8 ) ) , TemplateMode.HTML, theVariables);
		final Document theDocument = XMLResource.load(new InputSource( theXhtmlIs )).getDocument();
		Utils.close( theXhtmlIs );
		
		final ITextRenderer renderer = new ITextRenderer();
		final SharedContext theSharedContext = renderer.getSharedContext();
		
		final ResourceLoaderUserAgent resourceLoaderUserAgent = new ResourceLoaderUserAgent( renderer, theVariables );
		
		resourceLoaderUserAgent.setSharedContext( theSharedContext );
		theSharedContext.setUserAgentCallback( resourceLoaderUserAgent );
		/*
		 * Seems that only a fixed set of meta names is provided
		 * Self created / names I have not seen in the raw PDF code
		 */
		renderer.getOutputDevice().addMetadata( "title" , inData.meta.title);
		renderer.getOutputDevice().addMetadata( "author" , inData.meta.author);
		renderer.getOutputDevice().addMetadata( "subject" , inData.meta.subject);
		renderer.getOutputDevice().addMetadata( "keywords" , inData.meta.keywords);
		renderer.getOutputDevice().addMetadata( "created-by" ,  getClass().getTypeName()+"_"+ LocalDateTime.now().toString() );
		
		renderer.setDocument( theDocument, "");
        renderer.layout();
        renderer.createPDF(inOs);
        
        inOs.flush();
	}
	
	private Map<String, Object> dataToVariables(T inData) {
		
		final Map<String,Object> theVariables = new HashMap<>();
		
		if ( inData == null ) {
			return theVariables;
		}
		
		inData.init();
		
		for( Entry<String,Field> aFieldEntry : dataFields.entrySet() ) {
			
			Object o;
			try {
				o = ReflectionUtils.getFieldValue(aFieldEntry.getValue(), inData);
			} catch (Exception e) {
				throw Utils.toRuntimeException(e);
			}
			
			//System.out.println( "put-f "+ aFieldEntry.getKey() +" = "+ o );
			theVariables.put( aFieldEntry.getKey(), o);
		}
		
		for( Entry<String,Method> aMethodEntry : dataMethods.entrySet() ) {
			
			Object o;
			try {
				o = ReflectionUtils.invokeMethod(aMethodEntry.getValue(), inData, Utils.EMPTY_CLASSES);
			} catch (Exception e) {
				throw Utils.toRuntimeException(e);
			}
			
			//System.out.println( "put-m "+ aMethodEntry.getKey() +" = "+ o );
			theVariables.put( aMethodEntry.getKey(), o);
		}
		
		if ( inData.postInit != null ) {
			
			inData.postInit.accept( theVariables );
		}
		
		return theVariables;
	}

	public InputStream render(T inData) throws Exception {
		
		final OutputInputStreams theOis = new OutputInputStreams();
		
		render( inData, theOis.outputStream );
		
		Utils.close( theOis.outputStream );
		
		return theOis.inputStream;
	}
	
	public byte[] renderAsBytes(T inData) throws Exception {
		final ByteArrayOutputStream baos= new ByteArrayOutputStream();
		render( inData, baos );
		return baos.toByteArray();
	}
	
	/**
	 * 
	 * @return creates a new DataObject for holding variables to be used in a rendering process.
	 */
	@SuppressWarnings("unchecked")
	public T newDataObject() {
		
		try {
		
			if ( dataObjectClass == null ) {
				throw new UnrecognizedOptionException( "no data-object class declared with "+ getClass().getTypeName() ); 
			}
			
			final DataObject theDataObject = dataObjectClass.newInstance();
			return (T)theDataObject;
			
		} catch (Exception e) {
			throw Utils.toRuntimeException( e );
		}
	}
	
	private InputStream runThymeleaf(InputStream inTemplateStream, TemplateMode inTemplateMode, Map<String,Object> inVariables) {
		
		try {
		
			final String theTemplate = Utils.inputStreamToString(inTemplateStream, Utils.CHARSET_UTF8, true);
			
			final TemplateSpec theTemplateSpec = new TemplateSpec( theTemplate, inTemplateMode );
			
			final Context thymeleafContext = new Context();
			thymeleafContext.setVariables( inVariables );
			
			final String theOutput = THYMELEAF_TEMPLATEENGINE.process( theTemplateSpec , thymeleafContext); 
			
			//System.out.println( theOutput );
			
			return new ByteArrayInputStream( theOutput.getBytes( Utils.CHARSET_UTF8 ) );
		
		} catch(Exception e) {

			throw Utils.toRuntimeException(e);
		}
	}
	
	public void render(T inData, File inFile) throws Exception {
		
		if ( inFile == null ) { return; }
		else if ( inFile.isDirectory() ) { throw new IllegalArgumentException("given file is a dir: "+ inFile); }
		
		Utils.mkParentDirs( inFile );
		
		try( OutputStream os = new FileOutputStream( inFile ) ) {
			
			render( inData, os );
		}
	}
	
	public interface ITemplate<T> {
		
		URL getHtml();
		InputStream getUriStream(String inUri);
		Class<T> getDataObjectClass();
	}
	
	/**
	 * Template template based for resource lookups on the package of the implementing class.<br/>
	 * Resources are expected within the same package.<br/>
	 * @param <T>
	 */
	public static abstract class Template<T extends PdfLetterRenderer.DataObject> implements ITemplate<T> {
		
		public static final String MARKERLINE_PAGEBREAK = "<br clear=\"page\"/>";
		public static final String HTML_PAGEBREAK = "<p style=\"page-break-before: always;\"/>";
		
		private String uriLookupBasepath;
		private final Class<T> dataObjectClass;
		private URL htmlTemplateUrl;
		
		public Template(String inHtmlTemplate, Class<T> dataObjectClass) {
			
			Utils.requireNotNull(dataObjectClass, "no data object class provided");
			
			this.dataObjectClass = dataObjectClass;
			
			uriLookupBasepath = dataObjectClass.getPackage().getName().replace('.', '/')+"/";
			
			htmlTemplateUrl = Utils.getResourceUrl( uriLookupBasepath+inHtmlTemplate );
			
			Utils.requireNotNull(htmlTemplateUrl, "no proper html template found at "+uriLookupBasepath+inHtmlTemplate);
		}
		
		@Override
		public final URL getHtml() {
			return htmlTemplateUrl;
		}

		public final InputStream getUriStream(String inUri) {
			return Utils.getResource( uriLookupBasepath+"/"+ inUri );
		}
		
		@Override
		public final Class<T> getDataObjectClass() {
			return dataObjectClass;
		}
		
		@Override
		public String toString() {
			return getClass().getTypeName()+" [template="+ htmlTemplateUrl +", dataObjectClass="+ dataObjectClass.getTypeName() +"]";
		}
	}
	
	public static String getHtmlSafeText(String inText) {
		
		if ( inText == null ) { return ""; }
		
		inText = inText.replaceAll("&", "&amp;"); // this needs to happen at very first
		inText = inText.replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll("\"", "&quot;");
		
		inText = inText.replaceAll("\r","").replaceAll("\n", "<br/>");
		inText = inText.replaceAll(" ", SPACE_ENTITY).replaceAll("\t", SPACE_ENTITY_TAB);
		
		return inText;
	}
	
	/**
	 * Template interface for the data which provides e.g. initialization functionalities
	 * @author greyshine
	 *
	 */
	public static abstract class DataObject implements Serializable {
		
		private static final long serialVersionUID = -3116759768389797456L;

		public final Meta meta = new Meta();

		Consumer<Map<String,Object>> postInit = null;
		
		/**
		 * Method called in the rendering process as step 1.
		 */
		public void init() {}
		
		/**
		 * Register callback method which is applied as last step of the init method.<br/>
		 * The init method is overwritten by an implementation of a PDF generator as part of an implemented {@link ITemplate}.<br/>
		 * On the other hand, the callback method postInit can be inserted at runtime by client rendering some data.  
		 * @param inConsumer
		 * @return
		 */
		public DataObject postInit(Consumer<Map<String,Object>> inConsumer ) {
			postInit = inConsumer;
			return this;
		}
	}

	private class ResourceLoaderUserAgent extends ITextUserAgent {
    	
    	final Map<String, Object> variables;

		public ResourceLoaderUserAgent(ITextRenderer renderer, Map<String,Object> inVariables) {
    		super( renderer.getOutputDevice() );
    		variables = inVariables;
        }

    	@Override
        protected InputStream resolveAndOpenStream(String uri) {
        	
        	if ( uri == null ) {
        		return super.resolveAndOpenStream(uri);
        	} 
        	
        	if ( uri.toLowerCase().trim().startsWith( "data:" ) ) {
        		return new ByteArrayInputStream( uri.getBytes( Utils.CHARSET_UTF8 ) );
        	}
        	
        	InputStream theIs = null;
        	
        	try {
				theIs = Utils.getResource( uriBasepath+uri );
			} catch (Exception e) {
				throw Utils.toRuntimeException(e);
			}
        	
        	if ( theIs == null ) {
        		
        		theIs = templateInformation.getUriStream( uri );
        	}
        	
        	if ( theIs != null && uri.toLowerCase().endsWith(".css") ) {

        		theIs = runThymeleaf( theIs, TemplateMode.CSS, variables );
        	}

        	return theIs != null || !allowInternetUrlStreaming ? theIs : super.resolveAndOpenStream(uri);
        }
    }
	
	@Retention( RetentionPolicy.RUNTIME )
	@Target( { ElementType.FIELD, ElementType.METHOD } )
	public @interface Variable {
		/**
		 * Name of the variable
		 * @return
		 */
		String value() default "";
	}
	
	public final static class Meta implements Serializable {
		
		public String title;
		public String subject;
		public String author;
		public String keywords;
		
	}
	
}
