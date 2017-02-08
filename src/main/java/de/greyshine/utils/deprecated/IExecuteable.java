package de.greyshine.utils.deprecated;

public interface IExecuteable {

	void execute() throws Exception;
	void exception(Exception inException);

	public static abstract class QuietExecutable implements IExecuteable {

		@Override
		public final void execute() throws Exception {
			
			doExecute();
		}
		
		public abstract void doExecute();

		@Override
		public void exception(Exception inException) {
		}
	}
	
}