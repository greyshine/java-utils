package de.greyshine.utils.beta;

import java.util.ArrayList;
import java.util.List;

public class Email {
	
	enum EAddressType {
		TO, BC, BCC
	}
	
	private List<Addressee> addresseesTo = new ArrayList<>();  
	private List<Addressee> addresseesBc = new ArrayList<>();  
	private List<Addressee> addresseesBcc = new ArrayList<>();  
	
	private String subject;
	private String plaintext;
	private String html;
	
	public Email() {
		
	}
	
	public class Attachment {
		
		private byte[] data;
	}
	
	public static class Addressee {
		
		private String email;
		private String name;
		public String getEmail() {
			return email;
		}
		public void setEmail(String email) {
			this.email = email;
		}
		public String getName() {
			return name;
		}
		public void setName(String name) {
			this.name = name;
		}
	} 
	
	public static class EmailBuilder {
		
		private Email email = new Email();
		
		public EmailBuilder to(String inEmail, String inName) {
			return addressee( EAddressType.TO , inName, inEmail);
		}
		public EmailBuilder bc(String inEmail, String inName) {
			return addressee( EAddressType.BC , inName, inEmail);
		}
		public EmailBuilder bcc(String inEmail, String inName) {
			return addressee( EAddressType.BCC , inName, inEmail);
		}
		
		public EmailBuilder addressee( EAddressType inAddressType, String inName, String inEmail ) {
			
			inAddressType = inAddressType == null ? EAddressType.TO : inAddressType;
			
			final Addressee a = new Addressee();
			a.email = inEmail;
			a.name = inName;
			
			switch ( inAddressType ) {
			case BC:
				email.addresseesBc.add( a );
				break;
			case BCC:
				email.addresseesBcc.add( a );
				break;
			case TO:
			default:
				email.addresseesTo.add( a );
				break;
			}
			
			return this;
		}
		
		
		public EmailBuilder reset() {
			
			email = new Email();
			return this;
		}
		
		public EmailBuilder subject(String inSubject) {
			
			email.subject = inSubject;
			return this;
		}
		
		public EmailBuilder plainText(String inPlainText) {
			email.plaintext = inPlainText;
			return this;
		}
		
		public EmailBuilder html(String inHtml) {
			email.html = inHtml;
			return this;
		}
		
		public Email create() {
			return email;
		}
	}
	
	
}
