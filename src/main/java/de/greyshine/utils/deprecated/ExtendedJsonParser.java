package de.greyshine.utils.deprecated;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import de.greyshine.utils.ReflectionUtils;

/**
 * functionalities are not thread safe
 */
public abstract class ExtendedJsonParser extends IJsonEventParser.Parser {

	/**
	 * An annotated method must be of return type void and have
	 */
	@Retention(RetentionPolicy.RUNTIME)
	@Target(ElementType.METHOD)
	public @interface Handle {

		/**
		 * path to handle
		 */
		String path() default "";

		String simplePath() default "";

		String regex() default "";

		String startsWith() default "";
	}

	private Map<Handle, Method> handleMethods = new LinkedHashMap<Handle, Method>();

	{
		final List<Method> theMethods = ReflectionUtils.collectMethods(getClass(), false, false, null, null, void.class, Handle.class, false);
		for (final Method aMethod : theMethods) {

			final Class<?>[] theParams = aMethod.getParameterTypes();
			if (theParams.length != 5) {
				continue;
			} else if (theParams[0] != String.class) {
				// path
				continue;
			} else if (theParams[1] != String.class) {
				// simple path
				continue;
			} else if (theParams[2] != String.class) {
				// name
				continue;
			} else if (theParams[3] != Integer.class) {
				// index
				continue;
			} else if (theParams[4] != boolean.class && theParams[4] != String.class && theParams[4] != BigDecimal.class && theParams[4] != Object.class) {
				// type
				continue;
			}

			System.out.println("registered: " + aMethod);

			handleMethods.put(aMethod.getAnnotation(Handle.class), aMethod);
		}
	}

	private void invokeHandle(String inPath, String inSimplePath, String inName, Integer inIndex, Object inValue) {

		for (final Handle aHandle : handleMethods.keySet()) {

			if (!aHandle.path().isEmpty() && !aHandle.path().equals(inPath)) {
				continue;
			}
			if (!aHandle.simplePath().isEmpty() && !aHandle.path().equals(inSimplePath)) {
				continue;
			}
			if (!aHandle.regex().isEmpty() && Utils.isNoMatch(inPath, aHandle.regex()) && !aHandle.regex().isEmpty() && Utils.isNoMatch(inSimplePath, aHandle.regex())) {
				continue;
			}
			if (!aHandle.startsWith().isEmpty() && !inPath.startsWith(aHandle.startsWith()) && !inSimplePath.startsWith(aHandle.startsWith())) {
				continue;
			}

			final Method theMethod = handleMethods.get(aHandle);
			
			if (inValue != null && !theMethod.getParameterTypes()[4].isAssignableFrom(inValue.getClass())) {
				continue;
			}

			try {

				ReflectionUtils.invokeMethod(theMethod, this, new Object[] { inPath, inSimplePath, inName, inIndex, inValue });

			} catch (final Exception e) {

				throw new RuntimeException("failed handling on " + handleMethods.get(aHandle), e);
			}
		}
	}

	@Override
	public final void handleNull(String inPath, String inSimplePath, String inName, Integer inIndex) {
		invokeHandle(inPath, inSimplePath, inName, inIndex, null);
	}

	@Override
	public final void handle(String inPath, String inSimplePath, String inName, Integer inIndex, String inText) {
		invokeHandle(inPath, inSimplePath, inName, inIndex, inText);
	}

	@Override
	public final void handle(String inPath, String inSimplePath, String inName, Integer inIndex, boolean inBoolean) {
		invokeHandle(inPath, inSimplePath, inName, inIndex, inBoolean);
	}

	@Override
	public final void handle(String inPath, String inSimplePath, String inName, Integer inIndex, BigDecimal inNumber) {
		invokeHandle(inPath, inSimplePath, inName, inIndex, inName);
	}
}
