package de.greyshine.utils.beta;

import java.util.ArrayList;
import java.util.List;

public class Result<T> {
	
	final List<String> messages = new ArrayList<>();
	
	private int code;
	private T object;
	private Exception exception;
	
	public Result(T inObject) {
		this( 0, inObject, null );
	}

	public Result(int inCode, T inObject) {
		this( inCode, inObject, null );
	}

	public Result(Exception inException) {
		this( 100, null, inException );
	}

	public Result(int inCode, Exception inException) {
		this( inCode, null, inException);
	}

	public Result(int inCode, T inObject, Exception inException) {
		code = inCode;
		object = inObject;
		exception = inException;
	}
	
	public boolean isError() {
		return code >= 100;
	}
	
	public boolean isExceptional() {
		return exception != null;
	}
	
	public int getCode() {
		return code;
	}

	public void setCode(int code) {
		this.code = code;
	}

	public T getObject() {
		return object;
	}

	public void setObject(T object) {
		this.object = object;
	}

	public Exception getException() {
		return exception;
	}

	public void setException(Exception exception) {
		this.exception = exception;
	}

	public List<String> getMessages() {
		return messages;
	}

	public void addMessage(String inMsg) {
		if ( inMsg != null ) {
			messages.add( inMsg );
		}
	}

}
