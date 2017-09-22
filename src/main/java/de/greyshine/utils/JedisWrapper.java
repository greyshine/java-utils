package de.greyshine.utils;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Function;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import redis.clients.jedis.Jedis;

public class JedisWrapper {
	
	private static final Log LOG = LogFactory.getLog( JedisWrapper.class );

	private AtomicLong jedisInstances = new AtomicLong();

	public enum Lock {

		/**
		 * Locks only writes. Reads may occur concurrently.
		 */
		WRITE,

		/**
		 * Locks any read and write.
		 */
		READ_WRITE;
	}

	private final Object SYNC_LOCK = new Object();
	private Lock currentLock = null;

	private static final ThreadLocal<Jedis> TL_JEDIS = new ThreadLocal<Jedis>() {

		@Override
		public void remove() {

			final Jedis theJedis = get();

			super.remove();

			if (theJedis == null) {
				return;
			}

			try {

				theJedis.close();

			} catch (Exception e) {

				e.printStackTrace();
			}
		}
	};

	private String password;
	private int port;
	private String host;
	private final long lockThreadWaitMillis = 777;
	private final long maxUnlockThreadWaitMillis = 20 * 1000;

	public Jedis getJedisThreadInstance() {

		Jedis theJedis = TL_JEDIS.get();

		if (theJedis == null) {

			theJedis = new Jedis(host, port) {

				final Long id = jedisInstances.incrementAndGet();

				@Override
				public String toString() {
					return "Jedis [id=" + id + ", host=" + host + ":" + port + ", super=" + super.toString() + "]";
				}
			};

			if (password != null && !password.trim().isEmpty()) {
				theJedis.auth(password);
			}

			TL_JEDIS.set(theJedis);
		}

		return theJedis;

	}

	public JedisWrapper(String inHost, String inPort) {
		this(inHost, inPort, null);
	}

	public JedisWrapper(String inHost, String inPort, String inPassword) {

		this.host = inHost;
		this.port = Integer.parseInt(inPort);
		this.password = inPassword;

		if ( !ping() ) {
			throw new RuntimeException("Cannot connect: " + host + ":" + port);
		}
		
		LOG.info( "connected: "+ host +":"+ port );
		System.out.println( "connected: "+ host +":"+ port );
	}

	public String get(String inKey) {
		return get(inKey, null);
	}

	public String get(String inKey, Lock inDemandingLock) {
		return executeLocked(inDemandingLock, (Jedis j) -> {
			return j.get( inKey );
		});
	}

	public <T> T executeLocked(Lock inDemandingLock, Function<Jedis, T> inFunction) {

		T theResult = null;

		handleLocking(inDemandingLock);

		theResult = inFunction.apply(getJedisThreadInstance());

		unlock();

		return theResult;
	}

	/**
	 * must be executed within synchronized context
	 * 
	 * @param inDemandingLock
	 */
	private void handleLocking(Lock inDemandingLock) {

		final long maxPassWaitTime = System.currentTimeMillis() + maxUnlockThreadWaitMillis;

		boolean demandsReadWrite = Lock.READ_WRITE == inDemandingLock;
		boolean demandsWrite = Lock.WRITE == inDemandingLock;

		boolean isPass = false;
		// read write always needs to wait
		while (!isPass) {

			synchronized (SYNC_LOCK) {

				final boolean isCurrentReadWrite = Lock.READ_WRITE == currentLock;
				final boolean isCurrentWrite = Lock.WRITE == currentLock;

				isPass = true;

				if (isCurrentReadWrite) {

					// currently exclusive lock by another thread
					isPass = false;

				} else if (demandsReadWrite && currentLock != null) {

					// asking for exclusive lock but another thread owns any
					// different lock
					isPass = false;

				} else if (demandsWrite && isCurrentWrite) {

					// asking for write lock and write is locked currently
					isPass = false;
				}

				if (!isPass) {

					if (System.currentTimeMillis() > maxPassWaitTime) {
						throw new IllegalStateException("Waited too long for unlock: " + Thread.currentThread());
					}

					try {

						wait(lockThreadWaitMillis);

					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}

				} else {
					// will break the while loop
				}
				
				
				currentLock = inDemandingLock;

			} // eof synchronize
		}
	}

	private void unlock() {
		synchronized(SYNC_LOCK) {
			currentLock = null;
			SYNC_LOCK.notifyAll();
		}
	}

	public void set(String inKey, String inValue) {
		set( Lock.WRITE, inKey, inValue );
	}

	public void set(Lock inDemandingLock, String inKey, String inValue) {
		
		inDemandingLock = inDemandingLock == null ? Lock.WRITE : inDemandingLock;
		
		executeLocked(inDemandingLock, (jedis)->{
			jedis.set(inKey, inValue);
			return null;
		});
	}
	
	public String setBinary(byte[] inData) throws IOException {
		final String theId = UUID.randomUUID().toString();
		setBinary( Lock.WRITE, theId, new ByteArrayInputStream( inData == null ? Utils.EMPTY_BYTES : inData ) , true );
		return theId;
	}

	public void setBinary(String inId, byte[] inData) throws IOException {
		setBinary( Lock.WRITE, inId, new ByteArrayInputStream( inData == null ? Utils.EMPTY_BYTES : inData ) , true );
	}
	
	public String setBinary(InputStream inIs, boolean inCloseStream) throws IOException {
		final String theId = UUID.randomUUID().toString();
		setBinary( Lock.WRITE, theId, inIs, inCloseStream );
		return theId;
	}

	public void setBinary(String inId, InputStream inIs, boolean inCloseStream) throws IOException {
		setBinary( Lock.WRITE, inId, inIs, inCloseStream );
	}
	
	public void setBinary(Lock inDemandingLock, String inKey, final InputStream inIs, boolean inCloseStream) throws IOException {
		
		final List<Integer> theBytes = new ArrayList<>();
		
		InputStream theIs = inIs != null ? new InputStream() {
			
			@Override
			public int read() throws IOException {
				
				int b = inIs.read();
				theBytes.add( b );
				return b;
			}

			@Override
			public int available() throws IOException {
				return inIs.available();
			}
			
		} : new ByteArrayInputStream( Utils.EMPTY_BYTES );
		
		
		final String theBase64 = Utils.toBase64( theIs, inCloseStream );
		
		set(inDemandingLock, inKey, theBase64 );
	}
	
	public byte[] getBinary( String inKey ) {
		return inKey == null ? null : getBinary( null, inKey );
	}
	
	public byte[] getBinary(Lock inDemandingLock, String inKey ) {
		
		if ( inKey == null ) { return null; }
		
		final String theBase64Data = get(inKey, inDemandingLock);
		
		if ( theBase64Data == null ) { return null; }
		
		final byte[] theBytes = Utils.toBytesFromBase64( theBase64Data );
		return theBytes;
	}
	
	public void close() {
		// TODO: implement
		System.out.println( "TODO: implement" );
	}

	public boolean ping() {
		
		try {

			return getJedisThreadInstance().ping().equals( "PONG" );
			
		} catch (Exception e) {
			// intended swallow
			return false;
		}
	}

	public List<String> keys(String inPattern) {
		
		final List<String> theKeys = new ArrayList<>();

		keys( inPattern, (inKey)->{ theKeys.add( inKey ); }, true );
		
		return theKeys;
	}
	
	public void keys(String inPattern, Consumer<String> inKeyConsumer, boolean isFailAllOnSingleFail) {
		
		if ( inKeyConsumer == null ) { return; }
		
		final String thePattern = inPattern == null ? "*" : inPattern;
		
		executeLocked( null , (jedis)->{
			
			jedis.keys( thePattern ).stream().forEach( (inKey)->{
		
				try {
					
					inKeyConsumer.accept( inKey );
					
				} catch (RuntimeException e) {

					if ( isFailAllOnSingleFail ) {
						throw e;
					}
					// TODO: implement strategy for inDemandingLock Exception
				}
			} );

			return null;
		} );
	}
	
	
	
}
