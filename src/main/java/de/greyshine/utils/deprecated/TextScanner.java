package de.greyshine.utils.deprecated;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class TextScanner {

	public interface Event {

		String getText();

		String name();
	}
	
	private int index = 0;
	private String text;
	private final Set<Event> events = new HashSet<Event>();
	private List<Event> lastEvents = new ArrayList<Event>();
	private Event event = null;
	private String textBefore = "";
	private String textAfter;

	public TextScanner(String inText) {
		this( inText, (Event[])null );
	}
	
	public TextScanner(String inText, Event... inEvents) {
		text = textAfter = inText == null ? "" : inText;
		
		if ( inEvents != null ) {
			
			for (Event e : inEvents) {
				if ( e != null ) {
					register(e);
				}
			}
		}
	}

	public TextScanner register(Event inEvent) {

		if (inEvent != null) {
			events.add(inEvent);
		}
		return this;
	}

	public TextScanner register(final String inName, final String inText) {

		return register(new Event() {

			@Override
			public String getText() {
				return inText;
			}

			@Override
			public String name() {
				return inName;
			}
			
			public String toString() {
				
				return "Event [name="+ inName +"]";
			}
		});
	}
	
	public boolean hasNext() {
		
//		System.out.println( "\nnext..." );
		
		if ( index >= text.length() ) { return false; }
		
		Integer nxtIdx = null;
		Event e = null;
		int newIdx = text.length();
		
		for (Event anEvent : events) {
			
			final String theNextText = anEvent.getText();
			if ( theNextText == null ) { continue; }
			
			final int eIdx = text.indexOf( theNextText, index);
			
			if ( eIdx > -1 && (nxtIdx == null || eIdx < nxtIdx ) ) {
//				System.out.println( "check on "+ anEvent +": "+ eIdx +": overwrite" );
				nxtIdx = eIdx;
				e = anEvent;
				textBefore = text.substring(index, eIdx);
				newIdx = eIdx+theNextText.length();
				textAfter = text.substring(newIdx);
			} else{ 
				
//				System.out.println( "check on "+ anEvent +": "+ eIdx +": skip" );
			}
		}
		
		event = e;
		index = newIdx;
		
		if ( event != null ) {
			
			lastEvents.add( event );
			return true;
		}
		
		return false;
	}

	public Event getEvent() {
		return event;
	}
	
	@SuppressWarnings("rawtypes")
	public <T> Enum<?> getEventEnum() {
		
		if ( event != null && !( event instanceof Enum<?>) ) {
			throw new IllegalStateException("Event is not enum: "+ event);
		}
		
		return (Enum) event;
	}

	public String getTextBefore() {
		
		return textBefore;
	}

	public String getTextAfter() {

		return textAfter;
	}
	
	public Event getEventBefore() {
		
		if ( lastEvents.size() < 2 ) { return null; }
		
		return lastEvents.get( lastEvents.size()-2 );
	}

	public static void main(String[] args) {

		String t = "The Fox jumps\n\rover the ${whatever} thing!";

		TextScanner s = new TextScanner(t);
		s.register("CARRIGAERETURN", "\r");
		s.register("NEWLINE", "\n");
		s.register("VAR_S", "${");
		s.register("VAR_E", "}");

		while (s.hasNext()) {

			System.out.println("\n\nevt: " + s.getEvent());
			System.out.println("txt-before:\n\t'" + s.getTextBefore()+"'");
			System.out.println("txt-after:\n\t'" + s.getTextAfter()+"'");

		}

		System.out.println("end: \n\t'" + s.getTextAfter() +"'" );
	}

	public void removeEvent(Event inEvent) {
		
		events.remove( inEvent );
	}

	public int getEventCount() {
		return lastEvents.size();
	}
}
