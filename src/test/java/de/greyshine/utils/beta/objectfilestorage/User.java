package de.greyshine.utils.beta.objectfilestorage;

import java.util.HashMap;
import java.util.Map;

public class User {

	public String id;
	public String login;
	public String password;
	
	public final Map<Address.EType, Address> addresses = new HashMap<>();
	
	@Override
	public int hashCode() {
		return id == null ? 0 : id.hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		User other = (User) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		return true;
	}
	
	
	
	
	
}
