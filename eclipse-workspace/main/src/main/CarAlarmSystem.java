package main;
public class CarAlarmSystem implements ICarAlarmSystem {
	public boolean opened = false;
	public boolean closed = true;
	public boolean locked = false;
	public boolean unlocked = true;
	public boolean flash = false;
	public boolean sound = false;
	public boolean armed = false;
	public int time = 0;
	private java.util.Timer alarmSoundingTimer;
	private java.util.Timer alarmFlashingTimer;
	private java.util.Timer alarmArmedTimer;
	
	@Override
	public void lock() {
		locked = true;
		unlocked = false;
	}
	@Override
	public void unlock() {
		locked = false;
		unlocked = true;
		sound = false;
		flash = false;
		armed = false;
		alarmSoundingTimer.cancel();
		alarmFlashingTimer.cancel();
		alarmArmedTimer.cancel();
	}
	@Override
	public void close() {
		closed = true;
		opened = false;
		
//		// turn armed after 2 sec
//		alarmArmedTimer = new java.util.Timer();
//		alarmArmedTimer.schedule( 
//		        new java.util.TimerTask() {
//		            @Override
//		            public void run() {
//		                armed = true;
//		            }
//		        },
//		        2000
//		);
		
	}
	@Override
	public void open() {
		opened = true;
		closed = false;
		
		if (armed && locked && !unlocked) {
			// make noise for 3 seconds
			sound = true;
			alarmSoundingTimer = new java.util.Timer();
			alarmSoundingTimer.schedule( 
			        new java.util.TimerTask() {
			            @Override
			            public void run() {
			                sound = false;
			            }
			        }, 
			        3000
			);
			
			// flash for 30 seconds
			flash = true;
			alarmFlashingTimer = new java.util.Timer();
			alarmFlashingTimer.schedule( 
			        new java.util.TimerTask() {
			            @Override
			            public void run() {
			                flash = false;
			            }
			        }, 
			        30000
			);
		} else {
			armed = false;
		}
	}
	
	public void printState () {
		//System.out.println("door state: " + door.toString());
		System.out.println("alarm armed: " + armed);
		System.out.println("flashing " + flash);
		System.out.println("souding " + sound);
		System.out.println("-");
	}
}
