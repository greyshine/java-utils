package de.greyshine.utils.beta;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.security.sasl.RealmCallback;

import de.greyshine.utils.Lock;

public class WordingResources {

	private Locale defaultCountry = Locale.getDefault();
	
	public long lastLoadTime = -1;
	private Long reloadInterval = null;	
	
	// <country[A-Z]{2}>,Map<key,value>
	private final Map<String,Map<String,String>> languageWordings = new HashMap<>();
	
	private Set<String> nullResults = null;
 		
	public WordingResources defaultCountry( Locale inLocale ) {
		defaultCountry = inLocale == null ? Locale.getDefault() : inLocale;
		return this;
	}
	
	public void initDebug(long inReloadInterval, boolean inCollectRequestedNullResults) {
		
		reloadInterval = inReloadInterval <= 0 ? null : inReloadInterval;
		nullResults = inCollectRequestedNullResults ? new HashSet<>() : null;
		
	}
	
	public synchronized void reload() {
		
		
		
	}
	
	public String get(Locale inLocale, String inKey) {
		
		if ( inKey == null ) {return null; }
	
		if ( reloadInterval != null && System.currentTimeMillis() > lastLoadTime+reloadInterval ) {
			reload();
		} 
		
		inLocale = inLocale == null ? Locale.getDefault() : inLocale;
		
		Map<String,String> theWordings = languageWordings.get( inLocale.getCountry() );
		
		if (theWordings == null ) {
			
			if ( nullResults != null && inKey != null ) {
				
				nullResults.add( inKey );
			}
			
			return inKey;
		}
		
		final String theValue = theWordings.get( inKey );
		
		if ( theValue == null && nullResults != null && inKey != null) {
			
			nullResults.add( inKey );
		}
		
		return theValue;
	}
	
	
}
