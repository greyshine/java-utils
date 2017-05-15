package de.greyshine.utils;

import static de.greyshine.utils.Utils.EMPTY_CLASSES;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.greyshine.utils.deprecated.Utils;

public abstract class ReflectionUtils {

	public final static Log LOG = LogFactory.getLog(ReflectionUtils.class);
	private static final Map<Class<?>, Object> DEFAULT_NATIVE_TYPES;
	public static final List<Field> EMPTY_FIELDS_LIST = Collections.unmodifiableList( new ArrayList<Field>(0) );

	static {

		final Map<Class<?>, Object> types = new HashMap<Class<?>, Object>();

		types.put(boolean.class, Boolean.FALSE);
		types.put(short.class, (short) 0);
		types.put(int.class, 0);
		types.put(long.class, 0L);
		types.put(float.class, 0.0);
		types.put(double.class, 0.0D);

		DEFAULT_NATIVE_TYPES = Collections.unmodifiableMap(types);
	}

	private ReflectionUtils() {
	}

	public static boolean isNumberClass(Class<?> inClass) {

		return inClass != null && (Number.class.isAssignableFrom(inClass) || inClass == long.class || inClass == double.class || inClass == float.class || inClass == int.class || inClass == short.class);
	}

	public static boolean isNumber(Object inValue) {

		return inValue != null && isNumberClass(inValue.getClass());
	}

	public static boolean isFieldType(Object inObject, String inFieldName, Class<?> inFieldClassToCheck, boolean checkExactMatch) {
	
		if ( inObject == null || inFieldName == null || inFieldClassToCheck == null ) {
			return false;
		}
		
		Class<?> theFieldClass = null;
		
		try {
			theFieldClass = inObject.getClass().getDeclaredField( inFieldName ).getType();
		} catch (Exception e) {
			
			return false;
		} 
		
		return checkExactMatch ? theFieldClass == inFieldClassToCheck : inFieldClassToCheck.isAssignableFrom( theFieldClass );
	}
	
	public static boolean isFieldValueNull(Field inField, Object inObject) throws IllegalArgumentException, IllegalAccessException {

		final boolean isAccessible = inField.isAccessible();
		if (!isAccessible) {
			inField.setAccessible(true);
		}

		try {

			return inField.get(inObject) == null;
		} finally {
			if (!isAccessible) {
				inField.setAccessible(false);
			}
		}
	}

	public static Object invokeMethod(Method inMethod, Object inObject, Object[] inArgs) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {

		final boolean isAccessible = inMethod.isAccessible();
		if (!isAccessible) {
			inMethod.setAccessible(true);
		}

		try {

			return inMethod.invoke(inObject, inArgs);

		} finally {

			// why reset this beahviour; will setAccessible(true) anyways again on next invocation
			//			if (!isAccessible) {
			//				inMethod.setAccessible(false);
			//			}
		}
	}

	public static void setFieldValue(String inField, Object inObject, Object inValue) throws IllegalArgumentException, IllegalAccessException, NoSuchFieldException, SecurityException {

		final Class<?> theClass = (Class<?>) (inObject instanceof Class ? inObject : inObject.getClass());

		final Field theField = theClass.getDeclaredField(inField);

		setFieldValue(theField, Modifier.isStatic(theField.getModifiers()) ? null : inObject, inValue);
	}

	public static void setFieldValue(Field inField, Object inObject, Object inValue) throws IllegalArgumentException, IllegalAccessException {

		final boolean isAccessible = inField.isAccessible();
		if (!isAccessible) {
			inField.setAccessible(true);
		}

		try {

			inField.set(inObject, inValue);
		} finally {
			// why reset this beahviour; will setAccessible(true) anyways again on next invocation
			//			if (!isAccessible) {
			//				inField.setAccessible(false);
			//			}
		}
	}

	@SuppressWarnings("unchecked")
	public static <T> T getFieldValue(Field inField, Object inObject) throws IllegalArgumentException, IllegalAccessException {

		final boolean isAccessible = inField.isAccessible();
		if (!isAccessible) {
			inField.setAccessible(true);
		}

		try {

			return (T) inField.get(inObject);
			
		} finally {
			if (!isAccessible) {
				inField.setAccessible(false);
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	public static <T> T getFieldValueSafe(String inFieldName, Object inObject) {
		
		try {

			return (T)getFieldValue(inFieldName, inObject );
			
		} catch (Exception e) {
			// swalloow
			return null;
		}
	}

	public static <T> T getFieldValue(String inFieldName, Object inObject) throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		
		final Field theField = inObject.getClass().getDeclaredField( inFieldName );
		
		return getFieldValue(theField, inObject);
	}

	public static boolean isPublicEmptyConstructor(Class<?> inClass) {

		if (inClass == null) {

			return false;

		} else if (inClass.getConstructors().length == 0) {

			return true;
		}

		for (final Constructor<?> c : inClass.getConstructors()) {

			if (Modifier.isPublic(c.getModifiers()) && c.getParameterTypes().length == 0) {

				return true;
			}
		}

		return false;
	}

	public static boolean isClassLoadable(String... inClasses) {

		for (final String aClass : inClasses) {

			try {

				Class.forName(aClass);

			} catch (final Exception e) {

				return false;
			}
		}

		return true;
	}

	public static boolean isDefaultConstructorAvailable(Class<?> inClass) {

		if (inClass == null || isAbstract(inClass)) {

			return false;
		}

		try {

			final Constructor<?>[] theCs = inClass.getConstructors();

			if (theCs == null || theCs.length == 0) {

				return true;
			}

			for (final Constructor<?> c : theCs) {

				if (Modifier.isPublic(c.getModifiers()) && c.getParameterTypes().length == 0) {

					return true;
				}
			}

		} catch (final Exception e) {

			// swallow
		}

		return false;
	}

	public static Class<?>[] getInterfaceClasses(Class<?> inClass, final boolean inTravers) {

		if (inTravers) {

			throw new UnsupportedOperationException("Not yet implemented to travers interfaces");
		}

		final Set<Class<?>> theClasses = new HashSet<Class<?>>();

		for (final Class<?> aClass : getClassHierarchy(inClass)) {

			for (final Class<?> anInterfaceClass : aClass.getInterfaces()) {

				theClasses.add(anInterfaceClass);
			}
		}

		return theClasses.toArray(EMPTY_CLASSES);

	}

	public final static List<Class<?>> getClassHierarchy(Class<?> inClass, final Class<? extends Annotation> inClassAnnotation, final boolean inIncludeObjectClass, boolean inSpecificToObject) {
		
		final List<Class<?>> theClasses = new ArrayList<Class<?>>(3);

		traversClassHierarchy(inClass, new IClassHandler() {

			@Override
			public boolean handle(Class<?> inClass) {

				if (inClass == Object.class && !inIncludeObjectClass) {

					return true;

				} else if (inClassAnnotation != null && inClass.getAnnotation(inClassAnnotation) == null) {

					return true;
				}

				theClasses.add(inClass);
				return true;
			}
		});

		if (!inSpecificToObject) {

			Collections.reverse(theClasses);
		}

		return theClasses;

	}

	/**
	 * Returns the class and its super classes inlucding at last {@link Object}
	 * .class
	 */
	public final static List<Class<?>> getClassHierarchy(Class<?> inClass) {

		return getClassHierarchy(inClass, null, true, true);
	}

	public static List<Field> getMemberFieldsByType(Class<?> inClass, final Class<?> inFieldType) {

		final List<Field> theFields = new ArrayList<Field>();

		traversFieldHierarchy(inClass, new IFieldHandler() {

			@Override
			public boolean handle(Field inField) {

				if (inField.getType().isAssignableFrom(inFieldType)) {
					theFields.add(inField);
				}

				return true;
			}
		});

		return theFields;
	}

	/**
	 * Traverses from the given {@link Class} towards {@link Object}.
	 * 
	 * @param inClass
	 * @param inClassHandler
	 */
	public static void traversClassHierarchy(Class<?> inClass, IClassHandler inClassHandler) {

		while (inClass != null) {

			final boolean isKeepOnTraversing = inClassHandler.handle(inClass);
			
			if ( !isKeepOnTraversing ) { return; }
			
			inClass = inClass.getSuperclass();
		}
	}

	/**
	 * @param inClass
	 * @param inMethodHandler
	 */
	public static void traversMethodHierarchy(Class<?> inClass, final IMethodHandler inMethodHandler) {

		traversClassHierarchy(inClass, new IClassHandler() {

			@Override
			public boolean handle(Class<?> inClass) {

				for (final Method inMethod : inClass.getDeclaredMethods()) {

					final boolean isKeepOnTraversing = inMethodHandler.handle(inMethod);
				
					if ( !isKeepOnTraversing ) { return false; }
				}
				
				return true;
			}
		});
	}

	public static List<Field> collectNonStaticNonFinalFields(Class<?> inClass, final IFieldHandler inFieldHandler, final Class<? extends Annotation> inAnnotationClass) {

		final List<Field> theFields = new ArrayList<Field>();

		traversFieldHierarchy(inClass, new FieldHandler() {

			{
				handleStatic = false;
				handleFinal = false;
				annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? EMPTY_CLASSES : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Field inField) {

				theFields.add(inField);
				return false;
			}
		});

		return theFields;
	}

	public static void traversFieldHierarchy(Class<?> inClass, final IFieldHandler inFieldHandler) {

		traversClassHierarchy(inClass, new IClassHandler() {

			@Override
			public boolean handle(Class<?> inClass) {

				for (final Field aField : inClass.getDeclaredFields()) {

					if (inFieldHandler.handle(aField) == false) {

						return false;
					}
				}

				return true;
			}
		});
	}

	public static List<Method> collectNonAbstractNonPublicNonStaticNoParametersReturnVoidMethods(Class<?> inClass, final Class<? extends Annotation> inAnnotationClass, final boolean inTraversInterfaces) {

		if (inTraversInterfaces) {

			throw new UnsupportedOperationException("Not yet implemented to travers interfaces");
		}

		final List<Method> methods = new ArrayList<Method>();

		traversMethodHierarchy(inClass, new MethodHandler() {

			{
				handleStatic = false;
				handlePublic = false;
				handleAbstract = false;
				handleOnlyReturnTypeVoid = true;
				handleOnlyNoParameters = true;
				annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? EMPTY_CLASSES : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Method inMethod) {

				methods.add(inMethod);
				return true;
			}
		});

		return methods;
	}

	@SuppressWarnings("unchecked")
	public static List<Method> collectMethods(Class<?> inClass, final Boolean inStatic, final Boolean inAbstract, final Boolean inPublic, final Boolean inPrivate, final Class<?> inReturnType, final Class<? extends Annotation> inAnnotationClass, boolean inTraversInterfaces) {

		if (inTraversInterfaces) {

			throw new UnsupportedOperationException("Not yet implemented to travers interfaces");
		}

		final List<Method> methods = new ArrayList<Method>();

		traversMethodHierarchy(inClass, new MethodHandler() {

			{
				handleStatic = inStatic;
				handleAbstract = inAbstract;
				handlePublic = inPublic;
				handlePrivate = inPrivate;
				returnType = inReturnType;
				annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? EMPTY_CLASSES : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Method inMethod) {

				if (inMethod.getReturnType() == inReturnType) {

					methods.add(inMethod);
				}

				return true;
			}
		});

		return methods;
	}

	public static List<Field> getAnnotatedMemberFields(final Class<?> inClass, final Class<? extends Annotation> inAnnotationClass) {

		final List<Field> theFields = new ArrayList<Field>();

		traversFieldHierarchy(inClass, new FieldHandler() {

			{
				handleStatic = false;
				annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? EMPTY_CLASSES : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Field inField) {

				theFields.add(inField);
				return true;
			}
		});

		return theFields;
	}

	public static List<Field> getAnnotatedFields(final Class<?> inClass, final Class<? extends Annotation> inAnnotationClass) {

		final List<Field> theFields = new ArrayList<Field>();

		traversFieldHierarchy(inClass, new FieldHandler() {

			{
				handleStatic = false;
				annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? EMPTY_CLASSES : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Field inField) {

				theFields.add(inField);
				return true;
			}
		});

		return theFields;
	}

	public static boolean isClassAnnotated(Class<?> inClass, final Class<? extends Annotation> inAnnotationClass, final boolean inTraversInterfaces) {

		if (inTraversInterfaces) {

			throw new UnsupportedOperationException("Not yet implemented to travers interfaces");
		}

		final Wrapper<Boolean> theWrapper = new Wrapper<Boolean>(false);

		traversClassHierarchy(inClass, new IClassHandler() {

			@Override
			public boolean handle(Class<?> inClass) {

				if (theWrapper.value != true && inClass.isAnnotationPresent(inAnnotationClass)) {
					theWrapper.value = true;
					return false;
				}

				return true;
			}
		});

		return theWrapper.value;
	}

	/**
	 * @param inClass
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> T newInstance(String inClassName, boolean inQuiet) {

		try {
			return (T) newInstance(loadClass(inClassName), inQuiet);
		} catch (ClassNotFoundException | ClassCastException e) {

			if (inQuiet) {
				return null;
			}

			throw Utils.toRuntimeException(e);
		}
	}

	public static <T> T newInstance(Class<? extends T> inClass, Class<?>[] inParameterTypes, Object[] inParameters) throws NoSuchMethodException, SecurityException, InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {

		if (inParameters == null || inParameters.length == 0) {

			return newInstance(inClass, false);
		}

		return inClass.getDeclaredConstructor(inParameterTypes).newInstance(inParameters);
	}

	/**
	 * @param inClass
	 * @return
	 */
	public static <T> T newInstance(Class<? extends T> inClass, boolean inQuiet) {

		try {

			if (inClass == null) {

				throw new IllegalArgumentException("class is null");
			}

			// TODO: get default constructor and make it accessible if it is not
			return inClass.newInstance();

		} catch (final Exception e) {

			if (!inQuiet) {

				throw Utils.toRuntimeException(e);
			}
		}

		return null;
	}

	@SuppressWarnings("unchecked")
	public static <T> Class<T> loadClass(String inClassName) throws ClassNotFoundException, ClassCastException {

		try {

			return (Class<T>) ClassLoader.getSystemClassLoader().loadClass(inClassName);

		} catch (final Exception e) {
			// swallow
		}

		return (Class<T>) Thread.currentThread().getContextClassLoader().loadClass(inClassName);
	}

	public static <T> Class<T> loadClassQuietly(String inClassName) {

		try {

			return loadClass(inClassName);

		} catch (final Exception e) {

			LOG.debug("Cannot load class " + inClassName, e);
			return null;
		}
	}

	public static List<Method> getAnnotatedMethods(Class<?> inClass, final Class<? extends Annotation> inAnnotationClass, final boolean inStatic, final boolean inAbstract, final boolean inTraversInterfaces) {

		if (inTraversInterfaces) {

			throw new UnsupportedOperationException("Not yet implemented to travers interfaces");
		}

		final List<Method> theMethods = new ArrayList<Method>();

		traversMethodHierarchy(inClass, new IMethodHandler() {

			@Override
			public boolean handle(Method inMethod) {

				final int m = inMethod.getModifiers();

				if (inStatic != Modifier.isStatic(m)) {

					return true;

				} else if (inAbstract != Modifier.isAbstract(m)) {

					return true;

				} else if (inAnnotationClass != null && inMethod.getAnnotation(inAnnotationClass) == null) {

					return true;
				}

				theMethods.add(inMethod);

				return true;
			}
		});

		return theMethods;
	}

	/**
	 * Travereses from the given {@link Class} towards {@link Object}.
	 * 
	 * @param inClass
	 * @param inTravers
	 * @param inStatic
	 * @param inAbstract
	 * @return
	 */
	public static List<Method> getMethods(final Class<?> inClass, final boolean inTravers, final boolean inStatic, final boolean inAbstract) {

		final List<Method> theMethods = new ArrayList<Method>();

		traversMethodHierarchy(inClass, new IMethodHandler() {

			@Override
			public boolean handle(Method inMethod) {

				if (!inTravers && inMethod.getDeclaringClass() != inClass) {

					return false;
				}

				final int m = inMethod.getModifiers();

				if (inStatic != Modifier.isStatic(m)) {

					return true;

				} else if (inAbstract != Modifier.isAbstract(m)) {

					return true;
				}

				theMethods.add(inMethod);

				return true;
			}
		});

		return theMethods;
	}

	public static boolean isMethodAnnotated(Method inMethod, Class<? extends Annotation> inAnnotationClass, boolean inTraversInterfaces) {

		return getMethodAnnotation(inMethod, inAnnotationClass, inTraversInterfaces) != null;
	}

	/**
	 * Finds an {@link Annotation} in the class, one of the superclasses or in
	 * an interface
	 * 
	 * @param inMethod
	 * @param inAnnotationClass
	 * @return
	 */
	public static <T extends Annotation> T getMethodAnnotation(final Method inMethod, final Class<T> inAnnotationClass, final boolean inTraversInterfaces) {

		if (inMethod == null || inAnnotationClass == null) {

			return null;
		}

		final T theAnnotation = inMethod.getAnnotation(inAnnotationClass);

		if (theAnnotation != null) {

			return theAnnotation;
		}

		final Wrapper<T> theResult = new Wrapper<>();

		traversClassHierarchy(inMethod.getDeclaringClass(), new IClassHandler() {

			boolean isEqual(Method inM1, Method inM2) {

				if (!inM1.getName().equals(inM2.getName())) {

					return false;
				}

				final Class<?>[] thePs1 = inM1.getParameterTypes();
				final Class<?>[] thePs2 = inM2.getParameterTypes();

				if (thePs1.length != thePs2.length) {

					return false;
				}

				for (int i = 0; i < thePs1.length; i++) {

					if (thePs1[i] != thePs2[i]) {
						return false;
					}
				}

				return true;
			}

			@Override
			public boolean handle(Class<?> inClass) {

				if (inClass == Object.class) {

					return false;
				}

				// check methods of a super class
				if (inClass != inMethod.getDeclaringClass()) {

					// if method was annotated the traversal would not have been calleds

					for (final Method aMethod : inClass.getDeclaredMethods()) {

						final T aAnnotation = aMethod.getAnnotation(inAnnotationClass);

						if (aAnnotation == null || isStatic(aMethod) || !isEqual(inMethod, aMethod)) {

							continue;
						}

						theResult.value = aAnnotation;

						return false;
					}
				}

				if (inTraversInterfaces) {

					for (final Class<?> anInterface : inClass.getInterfaces()) {

						for (final Method aMethod : anInterface.getDeclaredMethods()) {

							final T aAnnotation = aMethod.getAnnotation(inAnnotationClass);

							if (aAnnotation == null || isStatic(aMethod) || !isEqual(inMethod, aMethod)) {

								continue;
							}

							theResult.value = aAnnotation;

							return false;
						}
					}
				}

				return theResult.value == null;
			}

		});

		return theResult.value;
	}

	public static boolean isPublicStatic(Method inMethod) {

		if (inMethod == null) {
			return false;
		}

		final int m = inMethod.getModifiers();

		return Modifier.isPublic(m) && Modifier.isStatic(m);
	}

	public static boolean isPrivate(Class<?> inClass) {

		return inClass != null && Modifier.isPrivate(inClass.getModifiers());
	}

	public static boolean isStatic(Class<?> inClass) {

		return inClass != null && Modifier.isStatic(inClass.getModifiers());
	}

	public static boolean isAbstract(Class<?> inClass) {

		return inClass != null && Modifier.isAbstract(inClass.getModifiers());
	}

	public static boolean isPrivate(Method inMethod) {

		return inMethod != null && Modifier.isPrivate(inMethod.getModifiers());
	}

	public static boolean isStatic(Method inMethod) {

		return inMethod != null && Modifier.isStatic(inMethod.getModifiers());
	}

	public static boolean isPublic(Method inMethod) {

		return inMethod != null && Modifier.isPublic(inMethod.getModifiers());
	}

	public static boolean isAbstract(Method inMethod) {

		return inMethod != null && Modifier.isAbstract(inMethod.getModifiers());
	}

	public static boolean isPublic(Field inField) {
		
		return inField != null && Modifier.isPublic(inField.getModifiers());
	}

	public static boolean isPublicStatic(Field inField) {

		return isPublic(inField) && isStatic(inField) ;
	}
	
	public static boolean isPivate(Field inField) {

		return inField != null && Modifier.isPrivate(inField.getModifiers());
	}

	public static boolean isStatic(Field inField) {
		return inField != null && Modifier.isStatic(inField.getModifiers());
	}
	
	public static boolean isNative(Field inField) {
		return inField != null && Modifier.isNative(inField.getModifiers());
	}

	public static boolean isAbstract(Field inField) {
		return inField != null && Modifier.isAbstract(inField.getModifiers());
	}

	public static boolean isFinal(Field inField) {
		return inField != null && Modifier.isFinal(inField.getModifiers());
	}
	
	public static boolean isTransient(Field inField) {
		return inField != null && Modifier.isTransient(inField.getModifiers());
	}

	public static boolean isFinal(Method inMethod) {
		return inMethod != null && Modifier.isFinal(inMethod.getModifiers());
	}

	public static boolean isCollection(Object inValue) {
		
		return inValue != null && Collection.class.isAssignableFrom(inValue.getClass());
	}

	public static <T extends Annotation> T getClassAnnotation(Class<?> inClass, final Class<T> inAnnotation, boolean inTraversInterfaces) {

		if (inClass == null || inAnnotation == null) {
			return null;
		} else if (inTraversInterfaces == true) {

			throw new UnsupportedOperationException("Not yet implemented");
		}

		final Wrapper<T> theResult = new Wrapper<>();

		traversClassHierarchy(inClass, new IClassHandler() {

			@Override
			public boolean handle(Class<?> inClass) {

				if (inClass.isAnnotationPresent(inAnnotation)) {

					theResult.value = inClass.getAnnotation(inAnnotation);
					return false;
				}

				return true;
			}
		});

		return theResult.value;
	}

	public static List<Field> getFields(Class<?> inClass, final Class<?> inFieldType, final Boolean inStatic, final Boolean inFinal, final Class<? extends Annotation> inAnnotationClass) {

		final List<Field> theFields = new ArrayList<Field>();
		
		traversFieldHierarchy(inClass, new FieldHandler() {

			{
				super.fieldType = inFieldType;
				super.handleFinal = inFinal;
				super.handleStatic = inStatic;
				super.annotationClasses = (Class<? extends Annotation>[]) (inAnnotationClass == null ? null : new Class<?>[] { inAnnotationClass });
			}

			@Override
			public boolean doHandle(Field inField) {

				theFields.add(inField);
				return true;
			}
		});
			
		
		return theFields;
	}

	public static Method getMainMethod(Class<?> inClass) {

		try {

			final Method theMethod =
			inClass.getDeclaredMethod("main", new Class[] { String[].class });
			 
			return isPublicStatic(theMethod) ? theMethod : null;

		} catch (final Exception e) {

			LOG.warn("main method not found [class=" + (inClass == null ? null : inClass.getName()) + "]");
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public static <T> T getDefaultNativeValue(Class<? extends T> inType) {

		return (T) DEFAULT_NATIVE_TYPES.get(inType);
	}

	public static boolean isVoid(Method inMethod) {

		return isVoid(inMethod, false);
	}

	public static boolean isVoid(Method inMethod, boolean inSafe) {

		if (inMethod == null && !inSafe) {

			throw new IllegalArgumentException("no method given");
		}
		
		return inMethod != null && inMethod.getReturnType() == void.class;
	}

	/**
	 * Checks a field to have the given (if they are given) attributes.
	 * 
	 * @param inField
	 * @param inType
	 * @param isStatic
	 * @param isFinal
	 * @return
	 */
	public static boolean isFieldDeclaration(Field inField, Class<?> inType, Boolean isStatic, Boolean isFinal) {

		if (inField == null) {

			return false;

		} else if (inType != null && inField.getType() != inType) {

			return false;

		} else if (isStatic != null && isStatic(inField) != isStatic) {

			return false;

		} else if (isFinal != null && isFinal(inField) != isFinal) {

			return false;
		}

		return true;
	}

	public static void forEachInArray(Object inArray, IHandler<Object> inHandler) {

		final int len = Utils.defaultIfNull(getArrayLength(inArray), 0);

		if (inHandler == null) {
			return;
		}

		boolean isContinue = true;

		int handles = 0;
		for (int i = 0; i < len && isContinue; i++) {

			final Object theValue = Array.get(inArray, i);
			
			try {
				
				isContinue = inHandler.handle(theValue, i);
				
			} catch (final Exception e) {
				
				try {
					
					isContinue = inHandler.handleException(i, e, theValue);
					
				} catch (final Exception e2) {

					throw Utils.toRuntimeException(e2);
				}

			} finally {
				handles++;
			}
		}

		try {
			
			inHandler.done(handles);

		} catch (final Exception e) {

			LOG.error(e);

		}
	}

	public static Integer getArrayLength(Object inArray) {

		return inArray == null || !inArray.getClass().isArray() ? null : Array.getLength(inArray);
	}

	public static Class<?> getClassSafe(Object inValue) {
		return inValue == null ? null : inValue.getClass();
	}

	public static Method getMethod(Class<?> inClass, final String inMethodName, final Class<?>[] inParameterTypes ) {
		
		final Class<?>[] theParameterTypes = inParameterTypes != null ? inParameterTypes : Utils.EMPTY_CLASSES;
		
		final Wrapper<Method> theMethod = new Wrapper<>();
		
		traversMethodHierarchy(inClass, new IMethodHandler() {

			@Override
			public boolean handle(Method inMethod) {
				
				if ( !inMethod.getName().equals( inMethodName ) ) {
					
					return true;
					
				} else if ( inMethod.getParameterTypes().length != inParameterTypes.length ){
					
					return true;
				}
				
				final Class<?>[] theMethodParameterTypes = inMethod.getParameterTypes();
				
				for (int i = 0, l=theMethodParameterTypes.length; i < l; i++) {
					
					if ( !theMethodParameterTypes[i].isAssignableFrom( theParameterTypes[i] ) ) {
						
						return true;
					}
					
				}
				
				theMethod.value = inMethod;
				
				return false;
				
			}});
		
		return theMethod.value;
	}
	
}
