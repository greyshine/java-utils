package de.greyshine.utils.deprecated;

import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.mongodb.MongoClient;
import com.mongodb.MongoCredential;
import com.mongodb.ServerAddress;

public class MongoUnit {

	private static final List<MongoCredential> EMPTY_CREDENTIALS = Collections.unmodifiableList(new ArrayList<MongoCredential>(0));

	public final String unitname;
	// default DB
	public final String defaultDbName;
	private final MongoClient mongoClient;

	public MongoUnit(String inPropertiesKey, String inPropertiesValue) throws UnknownHostException {

		// load mongo client
		int idx = inPropertiesKey.indexOf('.');
		unitname = idx < 1 ? null : inPropertiesKey.substring(0, idx);

		if (!".address".equalsIgnoreCase(inPropertiesKey.substring(idx))) {

			mongoClient = null;
			defaultDbName = null;
			return;
		}

		String theUser = null;
		String thePassword = null;
		idx = inPropertiesValue.indexOf('@');

		if (idx > 2) {

			theUser = inPropertiesValue.substring(idx);
			inPropertiesValue = inPropertiesValue.substring(idx + 1);

			idx = theUser.indexOf(':');

			if (idx < 1) {
				theUser = null;
			} else {
				thePassword = Utils.trimToNull(theUser.substring(idx + 1));
				theUser = Utils.trimToNull(theUser.substring(idx));
			}
		}

		idx = inPropertiesValue.indexOf('/');
		defaultDbName = idx < 0 ? null : Utils.trimToNull(inPropertiesValue.substring(idx + 1));

		inPropertiesValue = idx < 0 ? inPropertiesValue : inPropertiesValue.substring(0, idx);

		// TODO add support to several addresses

		idx = inPropertiesValue.indexOf(':');

		final String address = idx < 0 ? null : Utils.trimToNull(inPropertiesValue.substring(0, idx));
		final Integer port = idx < 0 ? null : Utils.parseInteger(inPropertiesValue.substring(idx + 1));

		mongoClient = new MongoClient(Arrays.asList(new ServerAddress(address, port)), theUser == null || thePassword == null ? EMPTY_CREDENTIALS : Arrays.asList(MongoCredential.createPlainCredential(theUser, "$EXTERNAL", thePassword.toCharArray())));
	}

	public MongoClient getMongoClient() {
		return mongoClient;
	}

	public void close() {

		getMongoClient().close();
	}

	public boolean isValid() {
		return getMongoClient() != null;
	}

	@Override
	public String toString() {
		return "MongoUnit [unitname=" + unitname + ", defaultDbName=" + defaultDbName + ", mongoClient=" + getMongoClient() + "]";
	}

}
