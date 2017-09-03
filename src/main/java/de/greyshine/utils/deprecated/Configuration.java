package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

public class Configuration {

	private Map<String, Object> map = new HashMap<String, Object>(0);
	private Configuration next;
	private Date lastLoadDate;
	
	public Configuration() throws Exception {

		this(null);
	}

	public Configuration(Configuration inNextConfiguration) throws Exception {
		
		setNextConfiguration(inNextConfiguration);
		load();
	}

	public Configuration setNextConfiguration(Configuration inConfiguration) {

		// TODO check on cycling configurations
		next = inConfiguration;
		return next;
	}

	public void clear() {

		if (next != null) {
			next.clear();
		}

		map.clear();
	}

	public Date getLastLoadTime() {

		return lastLoadDate;
	}

	public void load() throws Exception {

		clear();

		if (next != null) {
			next.load();
		}

		lastLoadDate = Timer.getDate();
	}

	public void set(String inKey, Object inValue) {

		map.put(inKey, inValue);
	}
	
	public Object getValue(String inKey) {
		
		final Object theValue = map.get(inKey);
		return theValue != null || map == null ? theValue : map.get(inKey);
	}

	public boolean isKey(String inKey) {

		return getKeys().contains(inKey);
	}

	public boolean isValue(String inKey) {

		return getValue(inKey) != null;
	}

	public String getString(String inKey) {

		return getString(inKey, null);
	}

	public String getString(String inKey, String inDefault) {

		return Utils.toStringSafe(getValue(inKey), inDefault);
	}

	public Long getLong(String inKey) {

		return de.greyshine.utils.Utils.parseLong(getString(inKey), null);
	}

	public Integer getInteger(String inKey) {

		return de.greyshine.utils.Utils.parseInteger(getString(inKey), null);
	}

	public Boolean getBoolean(String inKey) {

		return de.greyshine.utils.Utils.parseBoolean(getString(inKey), null);
	}

	public List<String> getKeys() {

		final Set<String> theKeys = new HashSet<String>(map.keySet());
		if (next != null) {
			theKeys.addAll(next.getKeys());
		}
		final List<String> theResults = new ArrayList<String>(theKeys);
		Collections.sort(theResults);
		return theResults;
	}

	public List<String> getKeysWithPrefix(String inPrefix) {

		final List<String> theKeys = new ArrayList<String>();

		for (final String aKey : getKeys()) {

			if (aKey == null && inPrefix == null) {
				theKeys.add(aKey);
			} else if (aKey != null && aKey.startsWith(inPrefix)) {
				theKeys.add(aKey);
			}
		}

		return theKeys;
	}

	public List<String> getKeysWithPostfix(String inPostfix) {

		final List<String> theKeys = new ArrayList<String>();

		for (final String aKey : getKeys()) {

			if (aKey == null && inPostfix == null) {
				theKeys.add(aKey);
			} else if (aKey != null && aKey.endsWith(inPostfix)) {
				theKeys.add(aKey);
			}
		}

		return theKeys;
	}

	public List<String> getKeysMatchingRegex(String inRegex) {
		
		final List<String> theKeys = new ArrayList<String>();
		
		for (final String aKey : getKeys()) {
			
			if (Utils.isMatch(aKey, inRegex)) {
				theKeys.add(aKey);
			}
		}
		
		return theKeys;
	}

	public int getSize() {

		return map.size() + (next == null ? 0 : next.getSize());
	}

	public void merge(Configuration inConfiguration) {

		if (inConfiguration == null) {
			return;
		}
		for (final String aKey : inConfiguration.getKeys()) {

			set(aKey, inConfiguration.getValue(aKey));
		}
	}

	public void merge(Properties inProperties) {

		if (inProperties == null) {
			return;
		}

		for (final Object aKey : inProperties.keySet()) {

			final String theKey = aKey == null ? null : aKey.toString();
			set(theKey, inProperties.getProperty(theKey));
		}
	}
}