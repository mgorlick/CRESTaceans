import org.apache.http.HttpResponse;

public class HTTPResponseWrapper {
    public HttpResponse resp;
    public Object obj;
    public boolean isDone;

    public HTTPResponseWrapper(HttpResponse r, Object o) {
      this.resp = r;
      this.obj = o;
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
