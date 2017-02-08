package de.greyshine.utils.deprecated;

/**
 * Pay attention not call <code>wait()</code> but better call
 * <code>doWait()</code>. Same with <code>notify()</code> and
 * <code>notifyAll()</code>
 */
public abstract class WaitCondition {

	public static final long SECS5 = 5 * 1000;
	public static final long SECS10 = 10 * 1000;
	public static final long MINS1 = 60 * 1000;
	public static final long MINS5 = 10 * 60 * 1000;
	public static final long MINS10 = 10 * 60 * 1000;
	public static final long MINS30 = 30 * 60 * 1000;

	final String tostringValue;
	final Object synchronizeObject;
	final Long timeout;

	public WaitCondition(Object synchronizeObject) {

		this(synchronizeObject, null);
	}

	public WaitCondition(Object synchronizeObject, Long inTimeout) {

		if (synchronizeObject == null) {

			throw new IllegalArgumentException("Synchronize object must never be null");
		}

		this.synchronizeObject = synchronizeObject;
		this.timeout = inTimeout == null || inTimeout < 1 ? null : inTimeout;

		tostringValue = new Throwable().getStackTrace()[1].toString();
	}

	public abstract boolean isWaitNeeded();

	private final boolean isWaitingNeeded() {

		try {

			return isWaitNeeded();

		} catch (final Exception e) {

			throw Utils.toRuntimeException(e);
		}
	}

	public final void doWait() throws TimeoutException {

		final Long theEndTime = timeout == null ? null : timeout + System.currentTimeMillis();

		while (isWaitingNeeded()) {

			try {
				if (theEndTime != null && theEndTime <= System.currentTimeMillis()) {

					throw new TimeoutException(this);

				} else if (timeout == null) {

					// ok to wait forever

					synchronized (synchronizeObject) {

						synchronizeObject.wait();
					}

				} else {

					synchronized (synchronizeObject) {

						synchronizeObject.wait(timeout);
					}
				}
			} catch (final InterruptedException e) {

				// swallow timing got interrupted
			}
		}
	}

	public void doNotify() {

		synchronized (synchronizeObject) {

			synchronizeObject.notifyAll();
		}
	}

	@Override
	public String toString() {

		return getClass().getName() + " [createdAt=" + tostringValue + "]";
	}
}