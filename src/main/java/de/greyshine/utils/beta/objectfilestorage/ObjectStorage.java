package de.greyshine.utils.beta.objectfilestorage;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Field;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.function.Function;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import de.greyshine.utils.ReflectionUtils;
import de.greyshine.utils.Utils;
import de.greyshine.utils.beta.JsonPersister;

public class ObjectStorage implements IObjectStorage {

	static final Log LOG = LogFactory.getLog(ObjectStorage.class);

	private static final Charset UTF8 = Charset.forName( "UTF-8" );

	private AtomicInteger errornousWriteCount = new AtomicInteger(0);

	private final JsonPersister jsonPersister = new JsonPersister();
	private final Map<Class<?>, Model> models = new HashMap<>();
	private IIdGenerator idGenerator = DefaultIdGenerator.INSTANCE;
	private FileStorage fileStorage;

	private Cache<Object> objectCache = new Cache<>( new Cache.ILoader<Object>() {

		public Object load(String inId) throws IOException {
			return fileStorage.read( inId );
		}
	} );
	
	public ObjectStorage() throws IOException {
		this(new File("."));
	}

	public ObjectStorage(String inBaseDir) throws IOException {
		this(inBaseDir == null ? null : new File(inBaseDir));
	}

	public ObjectStorage(File inBaseDir) throws IOException {
		
		final File theBaseDir =  inBaseDir != null ? inBaseDir : new File(".", ObjectStorage.class.getPackage().getName()).getCanonicalFile();
		
		fileStorage = new FileStorage( theBaseDir );
	}
	

	@Override
	public <T> T read(Class<T> inClass, String inId) throws IOException {

		if (inClass == null || inId == null) {
			return null;
		}

		final String theFileId = getFileId(inClass, inId);

		return (T) objectCache.getObject( theFileId );
	}

	@Override
	public String save(Object inObject) throws IOException {

		if (inObject == null) {
			return null;
		}

		final String theId = ensureId(inObject);

		final String theFileId = getFileId(inObject.getClass(), theId);

		objectCache.setObject(theFileId, inObject, new Utils.IExecuter<Object>() {

			@Override
			public Object run() throws Exception {

				final String theJson = jsonPersister.getJsonString(inObject);

				fileStorage.write(theFileId, new ByteArrayInputStream( theJson.getBytes( Utils.CHARSET_UTF8 ) ), true);
				
				return inObject;
			}
			
		} );
		
		return theId;
	}

	private String getFileId(Class<?> inType, String inId) {
		
		// always use forward slash as ID!
		return inType.getName() + "/" + inId + ".json";
	}

	/**
	 * Checks and/or creates an existing id on the Object 
	 * @param inObject
	 * @return
	 */
	private String ensureId(Object inObject) {

		String theId = getId(inObject);

		if (theId == null) {

			setId(inObject, theId = idGenerator.generate());
		}

		return theId;
	}

	private Model getModel(Class<?> inClass) {

		Model theModel = models.get(inClass);

		if (theModel == null) {
			try {
				models.put(inClass, theModel = new Model(inClass));
			} catch (Exception e) {
				throw Utils.toRuntimeException(e);
			}
		}

		return theModel;
	}

	private class Model {

		final Class<?> clazz;
		final Field idField;

		Model(Class<?> inClass) throws NoSuchFieldException {

			clazz = inClass;

			idField = inClass.getDeclaredField("id");

			if (idField != null && idField.getType() != String.class) {
				throw new NoSuchFieldException("expected to find member id on " + inClass);
			} else if (idField != null && ReflectionUtils.isFinal(idField)) {
				throw new NoSuchFieldException("expected to find member id on " + inClass);
			} else if (idField != null && ReflectionUtils.isStatic(idField)) {
				throw new NoSuchFieldException("expected to find member id on " + inClass);
			}

			idField.setAccessible(true);
		}
	}

	private String getId(Object inObject) {

		if (inObject == null) {
			return null;
		}

		try {

			return (String) getModel(inObject.getClass()).idField.get(inObject);

		} catch (IllegalArgumentException | IllegalAccessException e) {
			throw Utils.toRuntimeException(e);
		}

	}

	private void setId(Object inObject, String inId) {

		if (inObject == null) {
			return;
		}

		try {

			getModel(inObject.getClass()).idField.set(inObject, inId);

		} catch (IllegalArgumentException | IllegalAccessException e) {

			throw Utils.toRuntimeException(e);
		}
	}

	@Override
	public long countEntries() {
		return countEntries(null);
	}

	@Override
	public long countEntries(Class<?> inType) {
		
		final AtomicLong theCount = new AtomicLong(0L);
		
		traversFiles( null, (file)->{ theCount.incrementAndGet(); } );
		
		return theCount.get();
	}

	private void traversFiles( Class<?> inType, Consumer<File> inConsumer ) {
		
		if ( inConsumer == null ) { return; }
		
		if ( inType == null ) {
			// travers itself but with first/class parameter
			listTypes().forEach( (aType)->{ traversFiles(aType, inConsumer); } );
			return;
		}

		final File theTypeDir = new File( fileStorage.getBaseDir(), inType.getName() );
		
		Utils.listFiles( theTypeDir, false ).forEach( inConsumer );
	}

	@Override
	public List<Class<?>> listTypes() {
		
		final List<Class<?>> theTypes = new ArrayList<>();
		
		Utils.listDirs( fileStorage.getBaseDir() ).forEach( (file)->{
			
			try {
				
				final Class<?> c = ReflectionUtils.loadClass( file.getName() );
				theTypes.add( c );
				
			} catch (Exception e) {
				// intended swallow
			}
		} );
		
		return theTypes;
	}

	@Override
	public long getStorageSize() {
		return getStorageSize(null);
	}

	@Override
	public long getStorageSize(Class<?> inType) {
		
		final AtomicLong theSize = new AtomicLong( 0L );

		traversFiles(inType, (f)->{ theSize.addAndGet( f.length() ); } ); 
		
		return theSize.get();
	}
	
	

	@Override
	public <T> List<T> filter(Class<T> inClass, Function<T, Boolean> inFilter) {
		
		if ( inClass == null || inFilter == null ) { return Collections.emptyList(); }
		
		final List<T> theList = new ArrayList<>();
		
		for(String anId : fileStorage.listIds( inClass.getName() ) ) {
		
			try {
				
				final T theObject = objectCache.getObjectCasted( anId );	
			
				if ( theObject != null && Utils.isTrue( inFilter.apply( theObject ) ) ) {
					
					theList.add( theObject );
				}
				
			} catch (Exception e) {
				
				throw Utils.toRuntimeException(e);
			}
		}
		
		return theList;
	}
	
	@Override
	public void close() throws IOException {

		
		
	}

	@Override
	public String toString() {
		return "ObjectFileStorage [fileStore=" + fileStorage + "]";
	}
}
