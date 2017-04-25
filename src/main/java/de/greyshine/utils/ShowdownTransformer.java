package de.greyshine.utils;

import java.io.InputStreamReader;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

/**
 * Transforms markdown text into html.
 * Uses the showdownjs implementation: <a href="https://github.com/showdownjs/showdown">https://github.com/showdownjs/showdown</a> 
 *
 */
public abstract class ShowdownTransformer {
	
	public static final String OPTION_simpleLineBreaks = "simpleLineBreaks";
	public static final String OPTION_tables = "tables";
	public static final String OPTION_strikethrough = "strikethrough";

	/**
	 * https://raw.githubusercontent.com/showdownjs/showdown/master/dist/showdown.js
	 * 
	 * I downloaded the version and put it as resource.
	 * 
	 */
	private static final String RESOURCE_SHOWDOWN_JS = "js/showdownjs/showdown-1.6.3.js";
	private static final String SHOWDOWNJS;
	
	static {
		
		String theShowdownJs = null;
		
		try (InputStreamReader isr = new InputStreamReader( Thread.currentThread().getContextClassLoader().getResourceAsStream( RESOURCE_SHOWDOWN_JS ), "UTF-8")) {

			final StringBuilder sb = new StringBuilder();

			while (isr.ready()) {
				sb.append((char) isr.read());
			}

			theShowdownJs = sb.toString();
		
		} catch (Exception e) {
			
			System.err.println( "unable to load javascript resource: "+ RESOURCE_SHOWDOWN_JS );
			
			e.printStackTrace();
		}
		
		SHOWDOWNJS = theShowdownJs;
	}
	
	private static final ThreadLocal<ScriptEngine> TL_JS_SCRIPTENGINES_SHOWDOWNJS = ThreadLocal.withInitial( ()-> { 
		
		try {
			
			final ScriptEngine theNashorn = new ScriptEngineManager().getEngineByName("nashorn");
			theNashorn.eval("var console = { log:function(msg){print(msg);} }");
			theNashorn.eval(SHOWDOWNJS);
			theNashorn.eval("var theShowdownJsConverter = new showdown.Converter();");
			theNashorn.eval("theShowdownJsConverter.setOption('"+ OPTION_tables +"',true);");
			theNashorn.eval("theShowdownJsConverter.setOption('"+ OPTION_strikethrough +"',true);");
			theNashorn.eval("theShowdownJsConverter.setOption('"+ OPTION_simpleLineBreaks +"',true);");
			
			return theNashorn;	
			
		} catch (Exception e) {
			throw e instanceof RuntimeException ? (RuntimeException)e : new RuntimeException(e);
		}
	} );
	
	private ShowdownTransformer() {}
	
	public static String toHtml(String inMarkdown) {
		
		if ( SHOWDOWNJS == null ) {
			throw new IllegalStateException("unable to load javascript resource: "+ RESOURCE_SHOWDOWN_JS);
		}
		
		final ScriptEngine theSe = TL_JS_SCRIPTENGINES_SHOWDOWNJS.get();
		
		try {
			final Object c = theSe.eval("theShowdownJsConverter");
			final Object r = ((Invocable) theSe).invokeMethod(c, "makeHtml", inMarkdown == null ? "" : inMarkdown);
			return (String)r;
		} catch (RuntimeException e) {
			throw e;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}
