package de.greyshine.utils.deprecated;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.bson.BSONObject;

import com.mongodb.BasicDBList;
import com.mongodb.BasicDBObject;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;

public abstract class MongoUtils {

	private static final Log LOG = LogFactory.getLog(MongoUtils.class);

	private MongoUtils() {
	}

	public final static DBObject DBO_EMPTY = unmodifiableDbo(new BasicDBObject(0));
	public final static String REGEX_MONGO_ID = "[a-fA-F0-9]{24}";
	public static final Pattern PATTERN_MONGO_ID = Pattern.compile( REGEX_MONGO_ID );

	public static final int DESC = -1;
	public static final int ASC = 1;

	public static boolean isMongoId(String inMongoId) {

		return inMongoId != null && Utils.isMatch(inMongoId, PATTERN_MONGO_ID);
	}
	
	public static DBObject unmodifiableDbo(final DBObject inDbo) {

		return inDbo == null ? null : new DBObject() {

			@SuppressWarnings({ "rawtypes", "unchecked" })
			@Override
			public Map toMap() {
				return Collections.unmodifiableMap(inDbo.toMap());
			}

			@Override
			public Object removeField(String key) {
				throw new UnsupportedOperationException();
			}

			@SuppressWarnings("rawtypes")
			@Override
			public void putAll(Map m) {
				throw new UnsupportedOperationException();
			}

			@Override
			public void putAll(BSONObject o) {
				throw new UnsupportedOperationException();
			}

			@Override
			public Object put(String key, Object v) {
				throw new UnsupportedOperationException();
			}

			@Override
			public Set<String> keySet() {
				return inDbo.keySet();
			}

			@Override
			public Object get(String key) {
				return inDbo.get(key);
			}

			@SuppressWarnings("deprecation")
			@Override
			public boolean containsKey(String s) {
				return inDbo.containsKey(s);
			}

			@Override
			public boolean containsField(String s) {
				return inDbo.containsField(s);
			}

			@Override
			public void markAsPartialObject() {
				throw new UnsupportedOperationException();
			}

			@Override
			public boolean isPartialObject() {
				return inDbo.isPartialObject();
			}

			@Override
			public String toString() {
				return toMap().toString();
			}
		};
	}

	public static BasicDBObject bdbo(String inKey, Object inValue) {
		return new BasicDBObject(inKey, inValue);
	}

	public static BasicDBObject bdboSet(DBObject inDbo) {
		return new BasicDBObject("$set", inDbo);
	}
	public static BasicDBObject bdboSet(String inKey, Object inValue) {
		return new BasicDBObject("$set", bdbo(inKey, inValue));
	}

	public static BasicDBObject bdboInc(String inKey, Number inValue) {
		return new BasicDBObject("$inc", bdbo(inKey, inValue));
	}

	public static BasicDBObject appendInc(BasicDBObject inBasis, String inKey, Number inValue) {
		return inBasis.append("$inc", bdbo(inKey, inValue));
	}

	public static BasicDBObject bdboDec(String inKey, Number inValue) {
		return new BasicDBObject("$dec", bdbo(inKey, inValue));
	}

	public static BasicDBObject appendDec(BasicDBObject inBasis, String inKey, Number inValue) {
		return inBasis.append("$dec", bdbo(inKey, inValue));
	}

	public static BasicDBObject or(DBObject... inDbo) {
		return bdbo("$or", inDbo);
	}

	public static BasicDBList list(DBObject... inDbo) {

		final BasicDBList theBdbo = new BasicDBList();

		if (inDbo != null) {

			for (final DBObject object : inDbo) {

				if (object != null) {

					theBdbo.add(object);
				}
			}
		}

		return theBdbo;
	}

	public static void close(DBCursor inDbc) {

		try {

			inDbc.close();

		} catch (final Exception e) {
			// swallow
		}

	}

	public static void close(Collection<DBCursor> inValues) {

		if (inValues != null) {

			for (final DBCursor dbCursor : inValues) {
				close(dbCursor);
			}
		}
	}

	public static void close(DBCursor... inValues) {

		if (inValues != null) {

			for (final DBCursor dbCursor : inValues) {
				close(dbCursor);
			}
		}
	}

	public static <T> T fillObjectByDbo(DBObject inDbo, T inEntity) {

		if (inEntity == null || inDbo == null) {
			return inEntity;
		}

		for (final Field aField : ReflectionUtils.getFields(inEntity.getClass(), null, false, false, null)) {

			final Object theValue = inDbo.get(aField.getName());

			try {

				ReflectionUtils.setFieldValue(aField, inEntity, theValue);

			} catch (final Exception e) {

				LOG.warn("Failed to set value [object=" + inEntity + ", value=" + theValue + "] ");
			}
		}

		return inEntity;
	}

	public static Pattern createLike(String inText, boolean inCaseInsensitive) {

		return createLike(inText, inCaseInsensitive, true, true);
	}

	public static Pattern createLike(String inText, boolean inCaseInsensitive, boolean inAnyAtStart, boolean inAnyAtEnd) {

		final StringBuilder theRegex = new StringBuilder();
		if (inAnyAtStart) {
			theRegex.append(".*");
		}
		theRegex.append(Matcher.quoteReplacement(inText == null ? "" : inText));
		if (inAnyAtEnd) {
			theRegex.append(".*");
		}

		// https://blogs.oracle.com/xuemingshen/entry/case_insensitive_matching_in_java
		return !inCaseInsensitive ? Pattern.compile(theRegex.toString()) : Pattern.compile(theRegex.toString(), Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
	}

	public static BasicDBList list(Object... inValues) {
		
		if ( inValues == null ) { return null; }
		
		final BasicDBList theDbl = new BasicDBList();
		
		for (Object object : theDbl) {
			theDbl.add( object );
		}

		return theDbl;
	}

}
