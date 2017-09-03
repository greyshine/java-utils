package de.greyshine.utils.beta.objectfilestorage;

public class Address {

	public enum EType {
		EMAIL, WWW
	}
	
	public String address;

	public Address address(String inAddress) {
		address = inAddress;
		return this;
	}
	
}
