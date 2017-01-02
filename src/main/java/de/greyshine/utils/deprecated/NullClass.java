package de.greyshine.utils.deprecated;

/**
 * Marker class which can be used to represent a <code>null</code> value in
 * annotations default values.<br/>
 * e.g.<br/>
 * <code>@SomeAnnoation { Class<?> value() default NullClass.class; }</code>
 * 
 * @author greyshine
 */
public final class NullClass {

	private NullClass() {
	}
}