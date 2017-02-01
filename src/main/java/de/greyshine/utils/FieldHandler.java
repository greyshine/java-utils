package de.greyshine.utils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

public abstract class FieldHandler implements IFieldHandler {
	
	protected Class<?> fieldType;
	protected Boolean handleStatic = false;
	protected Boolean handleFinal = null;
	protected Boolean handlePrivate = null;
	protected Boolean handlePublic = null;
	protected Boolean handleProtected = null;
	protected Class<? extends Annotation>[] annotationClasses;

	public FieldHandler() {}

	public FieldHandler(boolean handleStatic, boolean handleFinal, boolean handlePrivate, boolean handlePublic, boolean handleProtected) {

		this.handleStatic = handleStatic;
		this.handleFinal = handleFinal;
		this.handlePrivate = handlePrivate;
		this.handlePublic = handlePublic;
		this.handleProtected = handleProtected;
	}

	@Override
	public final boolean handle(Field inField) {

		final int modifiers = inField.getModifiers();

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
		} else if (fieldType != null && !fieldType.isAssignableFrom(inField.getType())) {
			return true;
		}

		if (annotationClasses != null) {

			for (final Class<? extends Annotation> anAnnotationClass : annotationClasses) {

				if (anAnnotationClass != null && !inField.isAnnotationPresent(anAnnotationClass)) {
					return true;
				}
			}
		}

		return doHandle(inField);
	}

	public abstract boolean doHandle(Field inField);
}