import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.protocol.HttpContext;

public class ShamHandlerWrapper {
    public HttpRequest req;
    public HttpResponse resp;
    public HttpContext context;
    public boolean isDone;

    public ShamHandlerWrapper(HttpRequest req, HttpResponse resp,
                              HttpContext c) {
        this.req = req;
        this.resp = resp;
        this.context = c;
        isDone = false;
    }

    public synchronized void completed() {
        isDone = true;
        notifyAll();
    }

    public synchronized void waitCompleted() {
        while (!isDone) {
            try {
                wait();
            } catch (java.lang.InterruptedException ie) {
                ie.printStackTrace();
            }
        }
    }
}
