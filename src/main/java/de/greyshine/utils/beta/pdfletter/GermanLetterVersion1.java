package de.greyshine.utils.beta.pdfletter;

import java.io.InputStream;
import java.net.URL;

import de.greyshine.utils.Utils;
import de.greyshine.utils.beta.pdfletter.PdfLetterRenderer.DataObject;
import de.greyshine.utils.beta.pdfletter.PdfLetterRenderer.ITemplate;
import de.greyshine.utils.beta.pdfletter.PdfLetterRenderer.Variable;

public class GermanLetterVersion1 implements ITemplate<GermanLetterVersion1.Data> {
	
	final String basepath = getClass().getPackage().getName().replace('.', '/');
	
	public GermanLetterVersion1() {}
	
	@Override
	public URL getHtml() {
		return Utils.getResourceUrl( basepath+"/din-a4.v1.html" );
	}

	@Override
	public InputStream getUriStream(String inName) {
		return Utils.getResource( basepath+"/"+ inName );
	}

	@Override
	public Class<?> getDataObjectClass() {
		return Data.class;
	}

	public static class Data implements DataObject {
		
		@Variable("image")
		public String topImageBase64;

		public String senderline1;
		public String senderline2;
		
		public String addressline1;
		public String addressline2;
		public String addressline3;
		public String addressline4;
		public String addressline5;
		public String addressline6;
		
		public String extraline1left;
		public String extraline1right;
		public String extraline2left;
		public String extraline2right;
		public String extraline3left;
		public String extraline3right;
		public String extraline4left;
		public String extraline4right;
		public String extraline5left;
		public String extraline5right;
		public String extraline6left;
		public String extraline6right;
		public String extraline7left;
		public String extraline7right;
		public String extraline8left;
		public String extraline8right;
		public String extraline9left;
		public String extraline9right;
		
		public String location;
		public String date;

		public String subjectline1;
		public String subjectline2;

		public String lettertext;
		
		public String footerleft;
		public String footerright;
		
		public boolean showPageNumbers = false;
		
		@Override
		public void init() {
			
			extraline1left = Utils.defaultIfBlank( extraline1left , "&#160;");
			extraline1right = Utils.defaultIfBlank( extraline1right , "&#160;");
			
			extraline2left = Utils.defaultIfBlank( extraline2left , "&#160;");
			extraline2right = Utils.defaultIfBlank( extraline2right , "&#160;");
			
			extraline3left = Utils.defaultIfBlank( extraline3left , "&#160;");
			extraline3right = Utils.defaultIfBlank( extraline3right , "&#160;");
			
			extraline4left = Utils.defaultIfBlank( extraline4left , "&#160;");
			extraline4right = Utils.defaultIfBlank( extraline4right , "&#160;");
			
			extraline5left = Utils.defaultIfBlank( extraline5left , "&#160;");
			extraline5right = Utils.defaultIfBlank( extraline5right , "&#160;");
			
			extraline6left = Utils.defaultIfBlank( extraline6left , "&#160;");
			extraline6right = Utils.defaultIfBlank( extraline6right , "&#160;");
			
			extraline7left = Utils.defaultIfBlank( extraline7left , "&#160;");
			extraline7right = Utils.defaultIfBlank( extraline7right , "&#160;");
			
			extraline8left = Utils.defaultIfBlank( extraline8left , "&#160;");
			extraline8right = Utils.defaultIfBlank( extraline8right , "&#160;");
			
			extraline8left = Utils.defaultIfBlank( extraline9left , "&#160;");
			extraline8right = Utils.defaultIfBlank( extraline9right , "&#160;");
			
			extraline9left = Utils.defaultIfBlank( extraline2left , "&#160;");
			extraline9right = Utils.defaultIfBlank( extraline2right , "&#160;");
			
		}

		@Variable("locationdate")
		public String locationDate() {
			
			if ( Utils.isAllNotBlank( location, date ) ) {
				
				return location +", "+ date;
			
			} else if ( Utils.isNotBlank(location) ) {
				
				return location;
			}
			
			return date;
		}
	}
}
