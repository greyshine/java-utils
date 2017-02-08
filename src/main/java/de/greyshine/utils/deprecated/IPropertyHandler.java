package de.greyshine.utils.deprecated;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;

public interface IPropertyHandler {

	enum EValueType {
		
		ROOT, PROPERTY, MAP_KEY, MAP_VALUE, ARRAY_VALUE, COLLECTION_VALUE
	}
	
	/**
	 * 
	 * @param inValueType
	 * @param inParent
	 * @param inField
	 * @param inScalarWrapper the parent {@link Array}, {@link Collection} or {@link Map}
	 * @param inIndex
	 * @param inValue
	 * @return
	 */
	boolean handle(EValueType inValueType, Object inParent, Field inField, Object inScalarWrapper, Object inIndex, Object inValue);
	
}
