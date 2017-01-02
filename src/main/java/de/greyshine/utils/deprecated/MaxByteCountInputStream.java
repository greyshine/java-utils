package de.greyshine.utils.deprecated;

import java.io.IOException;
import java.io.InputStream;

public class MaxByteCountInputStream extends ByteCountInputStream {

	private final Long max;
	private final InputStream is;

	public MaxByteCountInputStream(InputStream in, Long inMax) {

		this(in, 0L, inMax);
	}

	public MaxByteCountInputStream(InputStream in, Long inStartCount, Long inMax) {

		super(in, inStartCount == null ? 0L : inStartCount);

		max = inMax == null || inMax < 1 ? null : inMax;
		is = in;
	}

	@Override
	public void notifyBytesAdded(long inCount) throws IOException {

		if (max != null && getCount() > max) {

			throw new MaxByteCountException( max, is );
		}
	}
	
	public static class MaxByteCountException extends IOException {
		
		private static final long serialVersionUID = -7470942935592347469L;

		public MaxByteCountException( long inMax, InputStream inIs ) {
			super( "Max bytes to read reached [max="+ inMax +", inputStream="+ inIs +"]" );
		}
	}
}
