package de.greyshine.utils;

import java.lang.reflect.Method;

public interface IMethodHandler {

	boolean handle(Method inMethod);

	public static abstract class GetterMethodHandler extends MethodHandler {
		{
			super.handleOnPrefix = "get";
			super.handleOnlyNonReturnTypeVoid = true;
			super.handleOnlyNoParameters = true;
		}
	}

	public static abstract class SetterMethodHandler extends MethodHandler {
		{
			super.handleOnPrefix = "set";
			super.handleOnlyReturnTypeVoid = true;
			super.handleOnly1ParameterMethod = true;
		}
	}

}