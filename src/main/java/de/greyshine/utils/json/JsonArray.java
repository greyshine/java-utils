package de.greyshine.utils.json;

import java.util.ArrayList;
import java.util.List;

public class JsonArray extends Json {
	
	private List<Json> values = new ArrayList<>();

	public JsonArray() {
		
	}
	
	public JsonArray add( Json inJson ) {
	
		return this;
	}
	
	public int size() {
		return values.size();
	}
	

	
	
}
