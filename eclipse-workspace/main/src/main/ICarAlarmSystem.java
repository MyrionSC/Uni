package main;

public interface ICarAlarmSystem {
	public boolean opened = false;
	public boolean closed = true;
	public boolean locked = false;
	public boolean unlocked = true;
	public boolean flash = false;
	public boolean sound = false;
	public boolean armed = false;
	public int time = 0;
	
	public void lock();
	public void unlock();
	public void close();
	public void open();
}
