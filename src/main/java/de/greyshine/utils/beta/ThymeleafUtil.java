package de.greyshine.utils.beta;

import java.util.HashMap;
import java.util.Map;

import org.thymeleaf.TemplateEngine;
import org.thymeleaf.TemplateSpec;
import org.thymeleaf.context.Context;
import org.thymeleaf.templatemode.TemplateMode;

public abstract class ThymeleafUtil {

	private static final TemplateEngine templateEngine = new TemplateEngine(); 
	
	private ThymeleafUtil() {}

	public static String generateText(String inTemplate, Map<String,Object> inVariables) {
		return generate( inTemplate, TemplateMode.TEXT, inVariables );
	}
	
	public static String generateHtml(String inTemplate, Map<String,Object> inVariables) {
		return generate( inTemplate, TemplateMode.HTML, inVariables );
	}
	
	public static String generate(String inTemplate, TemplateMode inTemplateMode, Map<String,Object> inVariables) {
		
		final Context theContext = new Context();
		theContext.setVariables( inVariables == null ? new HashMap<>() : inVariables );
		
		TemplateSpec theTemplateSpec = new TemplateSpec( inTemplate == null ? "" : inTemplate,  inTemplateMode == null ? TemplateMode.TEXT : inTemplateMode );
		
		return templateEngine.process(theTemplateSpec, theContext);
	}
	
}
