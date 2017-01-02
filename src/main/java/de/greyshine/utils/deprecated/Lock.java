package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class Lock {

	static final Log LOG = LogFactory.getLog(Lock.class);

	private static List<LockItem> lockItems = new ArrayList<LockItem>(1);
	public static volatile long wait_interval = 10 * 1000;

	private Lock() {
	}

	public static void lock() {

		lock(null);
	}
	
	public static void lock(Object inObject) {

		try {

			lock(inObject, null);

		} catch (final TimeoutException e) {

			// will never happen
			throw new RuntimeException("Must never happen. Sorry.", e);
		}
	}

	public static void lock(Object inObject, Long inMaxWaitTime) throws TimeoutException {

		final Long maxTime = inMaxWaitTime == null || inMaxWaitTime < 1 ? null : System.currentTimeMillis() + inMaxWaitTime;

		if (maxTime != null) {

			LOG.debug("Will wait until " + maxTime + "; now=" + System.currentTimeMillis());
		}

		synchronized (lockItems) {

			while (isLock(inObject)) {

				if (maxTime != null && System.currentTimeMillis() >= maxTime) {

					unlock(inObject);
					throw new TimeoutException(inObject, System.currentTimeMillis() - maxTime, inMaxWaitTime);
				}

				try {

					lockItems.wait(maxTime != null ? Math.max(1, maxTime - System.currentTimeMillis() + 1) : wait_interval < 0 ? wait_interval : 10000);

				} catch (final InterruptedException e) {

					LOG.debug("recheck on locks for " + Thread.currentThread());
					// swallow
				}
			}

			lockItems.add(new LockItem(inObject));

			LOG.debug("locked " + inObject + " for " + Thread.currentThread());
		}
	}

	public static void unlock() {

		unlock(null);
	}

	public static void unlock(Object inObject) {

		synchronized (lockItems) {

			for (int i = lockItems.size() - 1; i >= 0; i--) {

				if (lockItems.get(i).isMatch(inObject)) {

					final LockItem theLockItem = lockItems.remove(i);
					LOG.debug("unlocked: " + theLockItem.object + " by " + Thread.currentThread());
					lockItems.notifyAll();
				}
			}
		}
	}

	private static boolean isLock(Object inObject) {

		final Thread theCt = Thread.currentThread();

		for (final LockItem aLockItem : lockItems) {

			if (aLockItem.thread != theCt && aLockItem.isMatch(inObject)) {

				LOG.debug("hit locked object " + aLockItem.object + "; " + Thread.currentThread());

				return true;
			}
		}

		return false;
	}

	private static class LockItem {

		final Thread thread = Thread.currentThread();
		final Object object;

		private LockItem(Object object) {

			this.object = object;
		}

		boolean isMatch(Object inObject) {

			if (inObject == object) {

				return true;

			} else if (object == null && inObject != null) {

				return false;
			}

			return object.equals(inObject);
		}
	}

	public static class TimeoutException extends Exception {

		private static final long serialVersionUID = -8493015794071913047L;

		public TimeoutException(Object inLock, long inOvertime, long inMaxWaitTime) {

			super("Timeout for waiting of unlock [object=" + inLock + ", overtime=" + inOvertime + ", waitTime=" + inMaxWaitTime + ", thread=" + Thread.currentThread() + "]");
		}
	}
}
