package de.greyshine.utils.beta.businessinfos;

import java.util.ArrayList;
import java.util.List;

public class Result<T> {
	
	final List<String> messages = new ArrayList<>();
	
	private IStatusCode statusCode;
	private T object;
	private Exception exception;
	
	public Result(T inObject) {
		this( null, inObject, null );
	}

	public Result(IStatusCode inStatusCode, T inObject) {
		this( inStatusCode, inObject, null );
	}

	public Result(IStatusCode inStatusCode) {
		this( inStatusCode, null, null );
	}

	public Result(Exception inException) {
		this( IStatusCode.Code.ERROR_UNKNOWN , null, inException );
	}

	public Result(IStatusCode inStatusCode, Exception inException) {
		this( inStatusCode, null, inException);
	}

	public Result(IStatusCode inStatusCode, T inObject, Exception inException) {
		
		statusCode = inStatusCode;
		object = inObject;
		exception = inException;
		
		if ( inStatusCode == null && inException == null ) {
		
			statusCode = IStatusCode.Code.UNKNOWN;
		
		} else if ( inStatusCode == null && inException != null ) {
			
			statusCode = IStatusCode.Code.ERROR_UNKNOWN;
		}
		
	}
	
	public Boolean isError() {
		return statusCode.isError();
	}
	
	public boolean isExceptional() {
		return exception != null;
	}
	
	public IStatusCode getStatusCode() {
		return statusCode;
	}
	
	public int getCodeNum() {
		return statusCode.num();
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

	@Override
	public String toString() {
		return getClass().getSimpleName() +" [statusCode="+ statusCode.num() +":"+ statusCode.name() +", errorState="+ statusCode.isError() +", object="+ object +"]";
	}
	
}
