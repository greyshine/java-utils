package de.greyshine.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.greyshine.utils.Utils.IExecuter;

public class Lock {

	static final Log LOG = LogFactory.getLog(Lock.class);

	private List<LockItem> lockedItems = new ArrayList<LockItem>(1);
	private Object syncObject = new Object();
	private static final long WAIT_UNLOCK_RECHECK_INTERVAL = 10 * 1000;

	public void lock() {
		lock(null);
	}
	
	public void lock(Object inObject) {

		try {

			lock(inObject, null);

		} catch (final TimeoutException e) {

			// will never happen
			throw new RuntimeException("Must never happen. Sorry.", e);
		}
	}
	
	public void lock(Object inObject, Long inMaxWaitTime) throws TimeoutException {

		final Long theMaxTime = inMaxWaitTime == null || inMaxWaitTime < 1 ? null : System.currentTimeMillis() + inMaxWaitTime;

		if (theMaxTime != null) {

			LOG.debug("Will wait until " + theMaxTime + "; now=" + System.currentTimeMillis());
		}

		synchronized ( syncObject ) {

			while (isLock(inObject)) {

				// if wanted - if a time is set
				// check if time to wait for unlocking ended
				if (theMaxTime != null && System.currentTimeMillis() >= theMaxTime) {

					throw new TimeoutException(inObject, System.currentTimeMillis() - theMaxTime, inMaxWaitTime);
				}
				
				try {

					final long timeToWaitUntilRecheck = theMaxTime != null ? Math.max(1, theMaxTime - System.currentTimeMillis() + 1) : WAIT_UNLOCK_RECHECK_INTERVAL < 0 ? WAIT_UNLOCK_RECHECK_INTERVAL : 10000;
					
					syncObject.wait( timeToWaitUntilRecheck );

				} catch (final InterruptedException e) {

					LOG.debug("recheck on locks for " + Thread.currentThread());
					// swallow
				}
			}
			
			// waiting for the unlocked object is done
			// no lock it yourself
			
			final LockItem theLockItem = new LockItem(inObject);

			lockedItems.add( theLockItem );

			LOG.debug("locked " + inObject + " for " + Thread.currentThread());
		}
	}

	public void unlock() {

		unlock(null, false);
	}

	public void unlock(Object inObject) {
		unlock( inObject, false );
	}
	
	public void unlock(Object inObject, boolean inForce) {

		synchronized (syncObject) {

			for (int i = 0, l = lockedItems.size(); i < l; i++) {

				final LockItem theLockItem = lockedItems.get( i );
				
				if ( theLockItem.object != inObject ) {
					
					continue;
				}
				
				if ( !inForce && theLockItem.thread != Thread.currentThread() ) {
					throw new IllegalStateException( "wrong thread unlocking object [object="+ theLockItem.object +", thread="+ Thread.currentThread() +", expectedThread="+ theLockItem.thread +"]" );
				}
				
				lockedItems.remove( theLockItem );
				LOG.debug("unlocked [forced="+ inForce +", object="+ theLockItem.object +", thread="+ Thread.currentThread() +"]");
				
				// intended == equals compare!
				if (lockedItems.get(i) == inObject ) {

					syncObject.notifyAll();
				}
			}
		}
	}

	/**
	 * executed in a synchronized(syncObject) scope!
	 * @param inObject
	 * @return
	 */
	private boolean isLock(Object inObject) {
		
		/**
		 * due to execution in a synchronized(lockObject) scope no concurrent list access must occur
		 */
		for (final LockItem aLockItem : lockedItems) {

			if (aLockItem == inObject) {

				LOG.debug("hit locked object " + aLockItem.object);

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
	}

	public static class TimeoutException extends Exception {

		private static final long serialVersionUID = -8493015794071913047L;

		public TimeoutException(Object inLock, long inOvertime, long inMaxWaitTime) {

			super("Timeout for waiting of unlock [object=" + inLock + ", overtime=" + inOvertime + ", waitTime=" + inMaxWaitTime + ", thread=" + Thread.currentThread() + "]");
		}
	}
	
	public <T> void doLocked( T inObject, IExecuter<T> inExecuter ) {
		
		if ( inExecuter == null ) { return; }
		
		lock( inObject );
		
		Utils.execute(inExecuter);
		
		
		
	}
}
