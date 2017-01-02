package de.greyshine.utils;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class WidthHeight {

	public enum EModel {
		
		BOX, PORTRAIT, LANDSCAPE;

		public static EModel get(int inWidth, int inHeight) {
			
			if ( inWidth > inHeight ) { return LANDSCAPE; }
			else if ( inWidth < inHeight ) { return PORTRAIT; }
			return null;
		}

		public boolean isLandscape() {
			
			return this == LANDSCAPE;
		}
		
		public boolean isPortrait() {
			
			return this == PORTRAIT;
		}
		
		public boolean isBox() {
			
			return this == BOX;
		}
		
	}
	
	private int width, height;
	private EModel model;
	
	public WidthHeight(int inWidth, int inHeight) {
		
		setWidthHeight( inWidth, inHeight );
	}

	public WidthHeight(String inWidthXHeight) {
		
		int w = 0, h = 0;
		
		try {
			
			inWidthXHeight = inWidthXHeight.toLowerCase();
			String[] wh = inWidthXHeight.split( "x" , -1);
			if (wh.length != 2) { wh = null; } 
			w = Integer.parseInt( wh[0].trim() );
			h = Integer.parseInt( wh[1].trim() );
			
		} catch (Exception e) {
			
			throw new IllegalArgumentException("cannot parse 'width x height': "+ inWidthXHeight);
		}
		
		setWidthHeight(w,h);
	}

	public void setWidthHeight(int inWidth, int inHeight) {
		
		if ( inWidth <= 0 ) {
			
			throw new IllegalArgumentException("width must be > 0 [width="+ inWidth +", height="+ inHeight +"]");
		
		} else if ( inHeight <= 0 ) {
			
			throw new IllegalArgumentException("height must be > 0 [width="+ inWidth +", height="+ inHeight +"]");
		}
		
		this.width = inWidth;
		this.height = inHeight;
		model = EModel.get( width, height );
	}

	public int getWidth() {
		return width;
	}

	public void setWidth(int inWidth) {
		
		if ( inWidth <= 0 ) {
			
			throw new IllegalArgumentException("width must be > 0 [width="+ inWidth +", height="+ height +"]");
		}
		
		this.width = inWidth;
		model = EModel.get( width, height );
	}

	public int getHeight() {
		return height;
	}

	public void setHeight(int inHeight) {
		if ( inHeight <= 0 ) {
			
			throw new IllegalArgumentException("height must be > 0 [width="+ width +", height="+ inHeight +"]");
		}
		this.height = inHeight;
		model = EModel.get( width, inHeight );
	}

	public EModel getModel() {
		return model;
	}
	
	/**
	 * @param inScale how scale/significant the result will be. defaults to 2
	 * @return the division of width/height
	 */
	public double getAspectratio(Integer inScale) {
		
		if ( EModel.BOX == model ) { return 1.0; }
		
		inScale = inScale == null || inScale < 0 ? 2 : inScale; 
		
		return new BigDecimal( width ).divide( new BigDecimal( height ) , inScale+3 , RoundingMode.HALF_UP).setScale(inScale, RoundingMode.HALF_UP).doubleValue();
	}
	
	public double getAspectratio() {
		return getAspectratio( 2 );
	}
	
	@Override
	public String toString() {
		return "WidthHeight [width=" + width + ", height=" + height + ", model=" + model + ", aspectratio="+ getAspectratio() +"]";
	}

	public static void main(String[] args) {
		
		System.out.println( new WidthHeight(1024, 756) );
		System.out.println( new WidthHeight(756, 1024) );
		System.out.println( new WidthHeight(500, 300) );
		System.out.println( new WidthHeight(300,500) );
		System.out.println( new WidthHeight(1000, 500) );
		System.out.println( new WidthHeight(500,1000) );
		System.out.println( new WidthHeight(333,333) );
		System.out.println( new WidthHeight(-333,0) );
		
	}
	
}
