package de.greyshine.utils.deprecated;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public abstract class Executer {
	
	static final Log LOG = LogFactory.getLog( Executer.class );
	
	private volatile static long executionIds = 0;
	
	public static void execute(IExecuteable inExecuter) {

		doExecute(inExecuter, null, null);

	}

	public static void execute( Long inTimeout, IExecuteable inExecuter) {

		doExecute(inExecuter, inTimeout, null);
	}
	
	public static void executeSynchronized( Object inSyncObject, IExecuteable inExecuteable ) {
		
		doExecute( inExecuteable, null, inSyncObject );
	}
	public static void executeSynchronized( Long inTimeout, Object inSyncObject, IExecuteable inExecuteable ) {
		
		doExecute( inExecuteable, inTimeout, inSyncObject );
	}

	private static void doExecute(IExecuteable inExecuteable, Long inTimeout, Object inSyncObject) {

		if ( inExecuteable == null ) { return; }

		final long starttime = System.currentTimeMillis();
		
		final long executionId = ++executionIds;
		
		inTimeout = inTimeout == null || inTimeout < 1 ? null : inTimeout;
		
		// TODO add timer
		
		if ( LOG.isDebugEnabled() ) {
			
			LOG.debug( "["+ executionId +"][start][timeout="+ inTimeout +"][synchronize="+ inSyncObject +"]: "+ inExecuteable );
		}
		
		try {
			
			if ( inSyncObject == null ) {
				
				inExecuteable.execute();
				
			} else {
				
				synchronized (inSyncObject) {
					
					inExecuteable.execute();
				}
				
			}

		} catch (final Exception e) {

			try {
				
				inExecuteable.exception( e );
				
			} catch (Exception e2) {
				// swallow
				
				LOG.error( "["+ executionId +"][exception][timeout="+ inTimeout +"][synchronize="+ inSyncObject +"][durance="+ (System.currentTimeMillis()-starttime) +"]: "+ inExecuteable, e2 );
				throw Utils.toRuntimeException( e2 );
			}

		} finally {
			
			if ( LOG.isDebugEnabled() ) {
				
				LOG.debug( "["+ executionId +"][end][timeout="+ inTimeout +"][synchronize="+ inSyncObject +"][durance="+ (System.currentTimeMillis()-starttime) +"]: "+ inExecuteable );
			}
		}
	}
}
