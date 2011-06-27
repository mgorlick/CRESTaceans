import java.io.IOException;
import java.io.InterruptedIOException;

import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.nio.reactor.IOEventDispatch;

public class NIO_HTTP_Client_Thread implements Runnable {
    DefaultConnectingIOReactor io_reactor;
    IOEventDispatch io_event_dispatch;

    public NIO_HTTP_Client_Thread(DefaultConnectingIOReactor r, final IOEventDispatch d) {
	io_reactor = r;
	io_event_dispatch = d;
    }

    public void run() {
	try {
	    io_reactor.execute(io_event_dispatch);
	} catch (InterruptedIOException ex) {
	    System.err.println("Interrupted");
	} catch (IOException e) {
	    System.err.println("I/O error: " + e.getMessage());
	}
	System.out.println("Shutdown");
    }
}
