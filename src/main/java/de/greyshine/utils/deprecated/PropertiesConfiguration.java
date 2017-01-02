package de.greyshine.utils.deprecated;

import java.util.Properties;

public class PropertiesConfiguration extends Configuration {

	private final Properties properties = new Properties();

	public PropertiesConfiguration(Properties inProperties, Configuration inNextConfiguration) throws Exception {

		super(inNextConfiguration);
	}

	@Override
	public void load() throws Exception {

		merge(properties);
		super.load();
	}

}
