package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Timetracker {

	private final static Map<Object, List<Timetracker>> ALL = new HashMap<Object, List<Timetracker>>();

	private final static Map<Object, Integer> MAX_CONCURRENTS = new HashMap<Object, Integer>();

	private final static ThreadLocal<Map<Object, Timetracker>> TL_TRACKERS = new ThreadLocal<Map<Object, Timetracker>>() {
		@Override
		protected Map<Object, Timetracker> initialValue() {
			return new HashMap<Object, Timetracker>();
		}
	};

	public static int getAllMaxConcurrents() {

		int theMax = -1;

		synchronized (MAX_CONCURRENTS) {

			for (final Integer aValue : MAX_CONCURRENTS.values()) {

				theMax = Math.max(aValue, theMax);
			}
		}

		return theMax;
	}

	private Object key;
	private Long min = null;
	private Long max = null;
	private Long sum = 0L;
	private volatile Long calls = 0L;
	private volatile Long current = null;
	private int maxConcurrents = 0;

	private Timetracker(Object inKey) {

		key = inKey;
	}

	public Object getKey() {
		return key;
	}

	public long getTimeSum() {
		return sum;
	}

	public long getCalls() {
		return calls;
	}

	public long getMax() {
		return max;
	}

	public long getMin() {
		return min;
	}

	public long getAverage() {
		
		if ( calls == null || sum == 0L ) { return 0L; }
		return sum / calls;
	}
	
	public int getMaxConcurrents() {

		return maxConcurrents;
	}

	public static void startTrack(Object inKey) {

		Timetracker tt = TL_TRACKERS.get().get(inKey);
		if (tt == null) {

			TL_TRACKERS.get().put(inKey, tt = new Timetracker(inKey));

			synchronized (ALL) {

				List<Timetracker> theTts = ALL.get(inKey);
				if (theTts == null) {
					ALL.put(inKey, theTts = new ArrayList<Timetracker>(1));
				}
				theTts.add(tt);
				ALL.put(inKey, theTts);
			}
		}

		stopTrack(inKey);

		tt.current = System.currentTimeMillis();

		synchronized (MAX_CONCURRENTS) {

			int theCurrents = 0;
			for (final Timetracker aTt : ALL.get(inKey)) {

				if (aTt.current == null) {
					continue;
				}

				theCurrents++;
			}

			Integer theMaxConcurrents = MAX_CONCURRENTS.get(inKey);
			theMaxConcurrents = theMaxConcurrents == null ? theCurrents : Math.max(theMaxConcurrents, theCurrents);
			MAX_CONCURRENTS.put(inKey, theMaxConcurrents);
		}
	}

	public static long stopTrack(Object inKey) {

		final Timetracker tt = TL_TRACKERS.get().get(inKey);
		if (tt == null) {
			return -1L;
		}

		if (tt.current == null) {
			return -1;
		}

		final long theTime = System.currentTimeMillis() - tt.current;
		tt.calls++;
		tt.min = tt.min == null ? theTime : Math.min(tt.min, theTime);
		tt.max = tt.max == null ? theTime : Math.max(tt.max, theTime);
		tt.sum += theTime;
		tt.current = null;
		return theTime;
	}
	
	public static Timetracker get(Object inKey) {
		
		final List<Timetracker> tts = ALL.get( inKey );
		if ( tts == null ) { return null; }
		final Timetracker t = new Timetracker(inKey);
		
		for (final Timetracker aT : tts) {
				
			if (t.min == null) {

				t.min = aT.min;

			} else if (aT.min != null) {

				t.min = Math.min(t.min, aT.min);
			}
			if (t.max == null) {

				t.max = aT.max;

			} else if (aT.max != null) {

				t.max = Math.max(t.max, aT.max);
			}

			t.calls += aT.calls;
			t.sum += aT.sum;
		}
		
		t.maxConcurrents = !MAX_CONCURRENTS.containsKey(t.key) ? 0 : MAX_CONCURRENTS.get(t.key);

		return t;
	}

	@Override
	public String toString() {

		return getClass().getSimpleName() + " [key=" + key + ", calls=" + calls + ", time=" + sum + ", min=" + min + "; max=" + max + ", avg=" + getAverage() + ", maxConcurrents=" + maxConcurrents + "]";
	}
}
