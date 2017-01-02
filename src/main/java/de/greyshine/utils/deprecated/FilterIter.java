package de.greyshine.utils.deprecated;

import java.util.Iterator;

public class FilterIter<T> implements Iterable<T>, Iterator<T> {

	private Iterator<T> iterator;
	private final IIterFilter<T>[] filters;
	private boolean hasNext;
	private T next;
	
	public FilterIter( Iterable<T> inIterable, IIterFilter<T>... inFilters ) {
		
		this( inIterable == null ? null : inIterable.iterator(), inFilters );
	}
	
	@SuppressWarnings("unchecked")
	public FilterIter(Iterator<T> inIterator, final IIterFilter<T>... inFilters) {
		
		iterator = inIterator == null ? Utils.ITERATOR_NONE : inIterator;
		filters = inFilters == null ? new IIterFilter[0] : inFilters;
		
		scanNext();
	}

	private void scanNext() {
		
		All:
		while( iterator.hasNext() ) {
			
			T theNextCandidate = iterator.next();
			
			for (IIterFilter<T> aFilter : filters) {
				
				if ( aFilter != null && !aFilter.is(theNextCandidate) ) {
			
					continue All;
				}
			}
			
			next = theNextCandidate;
			hasNext = true;
			return;
		}
		
		hasNext = false;
		next = null;
	}

	@Override
	public Iterator<T> iterator() {
		return this;
	}

	@Override
	public boolean hasNext() {
		return hasNext;
	}

	@Override
	public T next() {
		
		if ( !hasNext ) { throw new IllegalStateException("no more elements"); }
		
		final T theNext = next;
		scanNext();
		return theNext;
	}
	
	public static interface IIterFilter<T> {
		boolean is(T inItem);
	}
	
	public static class OffsetIterFilter<T> implements IIterFilter<T> {

		private Long skips;
		
		public OffsetIterFilter(Long inOffset) {
			
			skips = inOffset;
		}
		
		@Override
		public boolean is(T inItem) {
			
			if ( skips == null || skips == 0 ) { return true; }
			else if ( skips < 0 ) { return false; }
			
			skips--;
			
			return false;
		}
	}
	
	public static class LengthIterFilter<T> implements IIterFilter<T> {

		private Long left;
		
		public LengthIterFilter(Long inLength) {
			
			left = inLength;
		}
		
		@Override
		public boolean is(T inItem) {
			
			if ( left == null ) { return true; }
			else if ( left <= 0 ) { return false; }
			
			left--;
			
			return true;
		}
	}
	
	@SuppressWarnings("unchecked")
	public static <T> Iterable<T> createOffsetLengthIterable(Iterator<T> inIterator, Long inOffset, Long inSize) {
		
		return new FilterIter<T>(inIterator, new OffsetIterFilter<T>( inOffset ), new LengthIterFilter<T>( inSize ));
	};

}
