package de.greyshine.utils.deprecated;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

public abstract class MethodHandler implements IMethodHandler {

	protected Boolean handleStatic = null;
	protected Boolean handleFinal = null;
	protected Boolean handlePrivate = null;
	protected Boolean handlePublic = null;
	protected Boolean handleProtected = null;
	protected Boolean handleAbstract = null;
	protected Boolean handleOnlyReturnTypeVoid = null;
	protected Boolean handleOnlyNonReturnTypeVoid = null;
	protected Boolean handleOnlyNoParameters = null;
	protected Boolean handleOnly1ParameterMethod = null;
	protected String handleOnPrefix = null;
	protected String handleOnPostfix = null;
	protected Class<?> returnType = null;
	protected Class<? extends Annotation>[] annotationClasses;

	@Override
	public final boolean handle(Method inMethod) {

		final int modifiers = inMethod.getModifiers();

		if (handleStatic != null && !Modifier.isStatic(modifiers) && handleStatic) {
			return true;
		} else if (handleFinal != null && !Modifier.isFinal(modifiers) && handleFinal) {
			return true;
		} else if (handlePrivate != null && !Modifier.isPrivate(modifiers) && handlePrivate) {
			return true;
		} else if (handlePublic != null && !Modifier.isPublic(modifiers) && handlePublic) {
			return true;
		} else if (handleProtected != null && !Modifier.isProtected(modifiers) && handleProtected) {
			return true;
		} else if (handleAbstract != null && !Modifier.isAbstract(modifiers) && handleAbstract) {
			return true;
		} else if (handleOnlyReturnTypeVoid != null && handleOnlyReturnTypeVoid && inMethod.getReturnType() != void.class) {
			return true;
		} else if (handleOnlyNonReturnTypeVoid != null && handleOnlyNonReturnTypeVoid && inMethod.getReturnType() == void.class) {
			return true;
		} else if (handleOnlyNoParameters != null && handleOnlyNoParameters && inMethod.getParameterTypes().length > 0) {
			return true;
		} else if (returnType != null && returnType != inMethod.getReturnType()) {
			return true;
		} else if (handleOnPrefix != null && !inMethod.getName().startsWith(handleOnPrefix)) {
			return true;
		} else if (handleOnPostfix != null && !inMethod.getName().endsWith(handleOnPostfix)) {
			return true;
		} else if (handleOnly1ParameterMethod != null && handleOnly1ParameterMethod && inMethod.getParameterTypes().length != 1) {
			return true;
		}

		if (annotationClasses != null) {

			for (final Class<? extends Annotation> anAnnotationClass : annotationClasses) {

				if (anAnnotationClass != null && !inMethod.isAnnotationPresent(anAnnotationClass)) {
					return true;
				}
			}
		}

		return doHandle(inMethod);
	}

	public abstract boolean doHandle(Method inMethod);
}
