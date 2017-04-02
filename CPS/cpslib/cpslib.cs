using System.Threading;

public interface Continuation<in T> {
	void Invoke(T v);
}
public interface Thunk<out T> {
	void Evaluate(Continuation<T> k);
}
public interface Function<out R, in A1> {
	void Apply(Continuation<R> k, Thunk<A1> a1);
}
public interface Function<out R, in A1, in A2> {
	void Apply(Continuation<R> k, Thunk<A1> a1, Thunk<A2> a2);
}
public interface Function<out R, in A1, in A2, in A3> {
	void Apply(Continuation<R> k, Thunk<A1> a1, Thunk<A2> a2, Thunk<A3> a3);
}
public class StaticThunk<T> : Thunk<T> {
	private T m_value;

	public StaticThunk(T val) {
		m_value = val;
	}

	public void Evaluate(Continuation<T> k) {
		k.Invoke(m_value);
	}
}
public abstract class CachedThunk<T> : Thunk<T>, Continuation<T> {
	ManualResetEvent m_evtDone;
	System.Int32 m_bRunning;
	Continuation<T> m_kNext;
	T m_value;

	public CachedThunk() {
		m_evtDone = new ManualResetEvent(false);
		m_bRunning = 0;
	}

	public void Evaluate(Continuation<T> k) {
		ManualResetEvent evtDone = m_evtDone;
		if (evtDone != null) {
			int bRunning = Interlocked.Exchange(ref m_bRunning, 1);
			if (bRunning == 1) {
				evtDone.WaitOne();
				k.Invoke(m_value);
				return;
			} else {
				m_kNext = k;
				RealEvaluate(this);
				return;
			}
		} else {
			k.Invoke(m_value);
			return;
		}
	}

	public void Invoke(T val) {
		// Store the value
		m_value = val;
		// Set m_evtDone to null, so future Evaluate's will automatically use the cache value.
		ManualResetEvent evtDone = m_evtDone;
		m_evtDone = null;
		Continuation<T> kNext = m_kNext;
		m_kNext = null;
		// Trigger the old value of m_evtDone, so any waiting threads will proceed.
		evtDone.Set();
		// Clean up the thunk's upvalues
		Cleanup();
		// Pass on to actual continuation
		kNext.Invoke(val);
	}

	public abstract void RealEvaluate(Continuation<T> k);
	public abstract void Cleanup();
}
