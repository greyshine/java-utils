package de.greyshine.utils.deprecated;


public class TimeoutException extends Exception {

	private static final long serialVersionUID = -5659583568159210417L;

	public TimeoutException(String inMessage) {

		super(inMessage);
	}

	TimeoutException(long time, IExecuteable executer) {

		this("Timeout [time=" + time + ", executer=" + executer + "]");
	}

	TimeoutException(WaitCondition inWaitCondition) {

		this("Timeout [synchronized=" + inWaitCondition.synchronizeObject + ", timeout=" + inWaitCondition.timeout + ", WaitCondition=" + inWaitCondition + "]");

	}
}