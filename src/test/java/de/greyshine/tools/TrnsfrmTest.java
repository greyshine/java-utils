package de.greyshine.tools;

import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import de.greyshine.utils.SystemStreamCatcher;
import de.greyshine.utils.Utils;

public class TrnsfrmTest {
	
	static final String IMAGEPATH = "src/test/trnsfrm/image.png";
	static final String BASE64_IMAGE = "iVBORw0KGgoAAAANSUhEUgAAAD0AAAAVCAYAAAD1neayAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAASdEVYdFNvZnR3YXJlAEdyZWVuc2hvdF5VCAUAAADySURBVFhH5Y9bDsIwDARzdG4eCCLV4o4fkfgyH6O24/WjYzzG/DtQdgdld1B2B2V3UH6YM4Z6lCxbmbHQObtHnxE65wKlgZrDoS+y+iKrLyhDs72cdW9QGqqLlUpdn8RJLcreQGmggdud1jZZJuoljvIoDTQwOjqqbbJM1EvYfNiP0kAD1HkLvcVePnPbK57X2g2UBhqgrvKuWE85r3eTzQj7URqyoyrvyvIWylin2HqW/wKloXLU+q4cQs7zXnZR2eX6mwCqB1UOwSN+4E92hT+9mipovtq3eyivNaprhmqKzrlA2R2U3UHZHZTdQdmaMZ/kynGwtqbOiAAAAABJRU5ErkJg/WjYzzG/DtQdgdld1B2B2V3UH6YM4Z6lCxbmbHQObtHnxE65wKlgZrDoS+y+iKrLyhDs72cdW9QGqqLlUpdn8RJLcreQGmggdud1jZZJuoljvIoDTQwOjqqbbJM1EvYfNiP0kAD1HkLvcVePnPbK57X2g2UBhqgrvKuWE85r3eTzQj7URqyoyrvyvIWylin2HqW/wKloXLU+q4cQs7zXnZR2eX6mwCqB1UOwSN+4E92hT+9mipovtq3eyivNaprhmqKzrlA2R2U3UHZHZTdQdmaMZ/kynGwtqbOiAAAAABJRU5ErkJg/WjYzzG/DtQdgdld1B2B2V3UH6YM4Z6lCxbmbHQObtHnxE65wKlgZrDoS+y+iKrLyhDs72cdW9QGqqLlUpdn8RJLcreQGmggdud1jZZJuoljvIoDTQwOjqqbbJM1EvYfNiP0kAD1HkLvcVePnPbK57X2g2UBhqgrvKuWE85r3eTzQj7URqyoyrvyvIWylin2HqW/wKloXLU+q4cQs7zXnZR2eX6mwCqB1UOwSN+4E92hT+9mipovtq3eyivNaprhmqKzrlA2R2U3UHZHZTdQdmaMZ/kynGwtqbOiAAAAABJRU5ErkJg";
	
	@BeforeClass
	public static void beforeClass() {
		Assert.assertTrue( Utils.isFile( IMAGEPATH ) );
	}
	
	@Test
	public void testBase64Image() throws Exception {
		
		final String[] mainArgs = {"-q", "--file", IMAGEPATH, "BASE64"}; 
		
		final SystemStreamCatcher ssc = new SystemStreamCatcher(false);
		Trnsfrm.main( mainArgs );
		ssc.end();
		
		final String theOut = ssc.getDataSystemOutAsString(); 
		
		theOut.chars().map( (i)->{return (char)i;} ).forEach( 
			(c)->{ 
				
				String s = ""+(char)c; 
				
				boolean im = Utils.isMatch( s , "[a-zA-Z0-9/+]");
				
				if ( im == false ) {
					System.out.println( "no: "+ s );
				}
			}
		);
		
		Assert.assertTrue("nomatch", Utils.isMatch(theOut, "[a-zA-Z0-9/+]{500,}") );
	}

	/**
	 * Yet to come...
	 * @throws Exception
	 */
	@Test
	@Ignore
	public void testBase64ImageWrapHtml() throws Exception {
		
		final String[] mainArgs = {"-q", "--file", IMAGEPATH, "-wh", "-wht", "image/png", "BASE64"}; 
		
		final SystemStreamCatcher ssc = new SystemStreamCatcher(false);
		Trnsfrm.main( mainArgs );
		ssc.end();
		
		final String theOut = ssc.getDataSystemOutAsString(); 
		
		Assert.assertTrue( theOut.startsWith( "<img src=\"data:;base64," ) );
		Assert.assertTrue( theOut.endsWith( "\" alt=\"\"/>" ) );
	}

}
