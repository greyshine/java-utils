package de.greyshine.utils;

import java.util.concurrent.TimeoutException;

import org.junit.Assert;
import org.junit.Test;

public class ExecuteTests {

	@Test
	public void executeTimeout() {

		Wrapper<InterruptedException> ie = new Wrapper<>();
		Wrapper<TimeoutException> toe = new Wrapper<>();
		Wrapper<Exception> ex = new Wrapper<>();

		try {
			Utils.executeWithTimeout(() -> {

				try {
					Thread.sleep(2000);
				} catch (InterruptedException e) {
					ie.value = e;
				}

				return null;

			}, 1000);
		} catch (TimeoutException e) {

			toe.value = e;

		} catch (Exception e) {

			ex.value = e;
		}

		System.out.println("ie: " + ie);
		System.out.println("toe: " + toe);
		
		Assert.assertTrue(toe.isNotNull());

		// check regular running interruption
		ie.setNull();
		toe.setNull();
		ex.setNull();

		try {
			Utils.executeWithTimeout(() -> {

				while (0 < System.currentTimeMillis()) {
//					System.out.println(Thread.currentThread().interrupted() + " :: "
//							+ Thread.currentThread().isInterrupted() + " :: " + new Date());
				}

				return null;

			}, 1000);

		} catch (TimeoutException e) {

			toe.value = e;

		} catch (Exception e) {

			ex.value = e;
		}

		Assert.assertTrue(toe.isNotNull());
		Assert.assertTrue(ex.isNull());

		// test no time out
		ie.setNull();
		toe.setNull();
		ex.setNull();

		try {
			Utils.executeWithTimeout(() -> {

				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					ie.value = e;
				}

				return null;

			}, 1000);
		} catch (TimeoutException e) {

			toe.value = e;

		} catch (Exception e) {

			ex.value = e;
		}

		Assert.assertTrue(ie.isNull());
		Assert.assertTrue(toe.isNull());
		Assert.assertTrue(ex.isNull());

		// test exception
		ie.setNull();
		toe.setNull();
		ex.setNull();
		
		try {
			Utils.executeWithTimeout(() -> {
				
				throw new RuntimeException("test if this is caught");
				
			}, 1000);
		} catch (TimeoutException e) {
			
			toe.value = e;
			
		} catch (Exception e) {
			
			ex.value = e;
		}
		
		Assert.assertTrue(ie.isNull());
		Assert.assertTrue(toe.isNull());
		Assert.assertTrue(ex.isNotNull());
	}

}
