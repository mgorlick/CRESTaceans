import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.InetSocketAddress;
import java.net.URI;

import org.apache.log4j.Logger;

import java.util.concurrent.LinkedBlockingQueue;

import org.apache.http.HttpEntity;
import org.apache.http.HttpException;
import org.apache.http.HttpHost;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.impl.nio.DefaultClientIOEventDispatch;
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.message.BasicHttpRequest;
import org.apache.http.nio.NHttpConnection;
import org.apache.http.nio.protocol.BufferingHttpClientHandler;
import org.apache.http.nio.protocol.EventListener;
import org.apache.http.nio.protocol.HttpRequestExecutionHandler;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.nio.reactor.IOEventDispatch;
import org.apache.http.nio.reactor.SessionRequest;
import org.apache.http.nio.reactor.SessionRequestCallback;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.params.HttpParams;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.ExecutionContext;
import org.apache.http.protocol.RequestConnControl;
import org.apache.http.protocol.RequestContent;
import org.apache.http.protocol.RequestExpectContinue;
import org.apache.http.protocol.RequestTargetHost;
import org.apache.http.protocol.RequestUserAgent;
import org.apache.http.util.EntityUtils;

public class ImposterWrapper {
    /*
    public static LinkedBlockingQueue<HTTPRequestWrapper> requestQueue
        = new LinkedBlockingQueue<HTTPRequestWrapper>(10);

    public static LinkedBlockingQueue<HTTPResponseWrapper> responseQueue
        = new LinkedBlockingQueue<HTTPResponseWrapper>(10);
    */

    public LinkedBlockingQueue<HTTPRequestWrapper> requestQueue;
    public LinkedBlockingQueue<HTTPResponseWrapper> responseQueue;

    ConnectingIOReactor ioReactor;
    IOEventDispatch ioEventDispatch;

    Thread ioDispatchThread;
    Thread requestQueueThread;

    Logger logger;

    boolean shuttingDown;

    public ImposterWrapper(String user_agent) throws Exception {
        init(user_agent, Logger.getRootLogger());
    }

    public ImposterWrapper(String user_agent, Logger l) throws Exception {
        init(user_agent, l);
    }

    public void init(String user_agent, Logger l) throws Exception {

        logger = l;

        // Construct the long-lived HTTP parameters.
        HttpParams parameters = new BasicHttpParams();
        parameters
            // Socket data timeout is 5,000 milliseconds (5 seconds).
            .setIntParameter(CoreConnectionPNames.SO_TIMEOUT,         5000)
            // Maximum time allowed for connection establishment is 10,00 milliseconds (10 seconds).
            .setIntParameter(CoreConnectionPNames.CONNECTION_TIMEOUT, 10000)
            // Socket buffer size is 8 kB.
            .setIntParameter(CoreConnectionPNames.SOCKET_BUFFER_SIZE, 8 * 1024)
            // Don't bother to check for stale TCP connections.
            .setBooleanParameter(CoreConnectionPNames.STALE_CONNECTION_CHECK, false)
            // Don't use Nagle's algorithm (in other words minimize latency).
            .setBooleanParameter(CoreConnectionPNames.TCP_NODELAY,            true)
            // Set the user agent string that the client sends to the server.
            .setParameter(CoreProtocolPNames.USER_AGENT, user_agent);

        // Construct the core HTTP request processor.
        BasicHttpProcessor http_processor = new BasicHttpProcessor();
        // Add Content-Length header to request where appropriate.
        http_processor.addInterceptor(new RequestContent());
        // Always include Host header in requests.
        http_processor.addInterceptor(new RequestTargetHost());
        // Maintain connection keep-alive by default.
        http_processor.addInterceptor(new RequestConnControl());
        // Include user agent information in each request.
        http_processor.addInterceptor(new RequestUserAgent());

        http_processor.addInterceptor(new RequestAcceptEncoding());
        http_processor.addInterceptor(new ResponseGzipUncompress());

        // Allocate an HTTP client handler.
        BufferingHttpClientHandler handler = new BufferingHttpClientHandler(
                http_processor,
                new ImposterExecutionHandler(),
                new DefaultConnectionReuseStrategy(),
                parameters);
        handler.setEventListener(new EventLogger(logger));

        requestQueue = new LinkedBlockingQueue<HTTPRequestWrapper>(10);
        responseQueue = new LinkedBlockingQueue<HTTPResponseWrapper>(10);
        // Use two worker threads for the IO reactor.
        ioReactor = new DefaultConnectingIOReactor(2, parameters);
        ioEventDispatch = new DefaultClientIOEventDispatch(handler,
                                                           parameters);


        shuttingDown = false;

        ioDispatchThread = new Thread(new Runnable() {
            public void run() {
                try {
                    ioReactor.execute(ioEventDispatch);
                } catch (InterruptedIOException ex) {
                    logger.warn("ImposterWrapper: I/O Dispatch Thread Interrupted");
                } catch (IOException e) {
                    logger.error("ImposterWrapper: I/O Dispatch Thread Error: " + e.getMessage());
                    e.getStackTrace();
                }
                logger.info("ImposterWrapper: I/O Dispatch Thread Shutdown");
            }
        });
        ioDispatchThread.start();

        requestQueueThread = new Thread(new Runnable() {
            public void run() {
                logger.info("ImposterWrapper: Request Queue Thread started");
                while (!shuttingDown) {
                    try {
                        HTTPRequestWrapper hrw = requestQueue.take();
                        InetSocketAddress isa;

                        logger.debug("ImposterWrapper: " +
                                     "Request Queue Thread picked up req: " +
                                     hrw);

                        if (hrw.uri.getPort() == -1) {
                            isa = new InetSocketAddress(hrw.uri.getHost(), 80);
                        } else {
                            isa = new InetSocketAddress(hrw.uri.getHost(),
                                                        hrw.uri.getPort());
                        }
                        ioReactor.connect(isa, null, hrw, null);
                    } catch (java.lang.InterruptedException ie) {
                        ie.printStackTrace();
                    }
                }
                logger.info("ImposterWrapper: Request Queue Thread shutdown");
            }
        });
        requestQueueThread.start();
    }

    class ImposterExecutionHandler implements HttpRequestExecutionHandler
    {
        private final static String HRW = "hrw";
        private final static String REQUEST_SENT       = "request-sent";
        private final static String RESPONSE_RECEIVED  = "response-received";

        public ImposterExecutionHandler() {
            super();
        }

        public void initalizeContext(final HttpContext context,
                                     final Object attachment) {
            context.setAttribute(HRW, attachment);
        }

        public void finalizeContext(final HttpContext context) {
            Object flag = context.getAttribute(RESPONSE_RECEIVED);
            if (flag == null) {
                // Signal completion of the request execution
            }
        }

        public HttpRequest submitRequest(final HttpContext context) {
            HTTPRequestWrapper hrw =
                (HTTPRequestWrapper)context.getAttribute(HRW);
            Object flag = context.getAttribute(REQUEST_SENT);
            if (flag == null) {
                context.setAttribute(REQUEST_SENT, Boolean.TRUE);
                logger.debug("ImposterWrapper: submitting request: " + hrw.req);

                return hrw.req;
            } else {
                // No new request to submit
                return null;
            }
        }

        public void handleResponse(final HttpResponse response,
                                   final HttpContext context) {
            HTTPRequestWrapper hrw =
                (HTTPRequestWrapper)context.getAttribute(HRW);

            context.setAttribute(RESPONSE_RECEIVED, Boolean.TRUE);

            HTTPResponseWrapper new_resp =
                new HTTPResponseWrapper(response, hrw.obj);

            logger.debug("ImposterWrapper: putting req on queue: " + new_resp);

            try {
                responseQueue.put(new_resp);
            } catch (java.lang.InterruptedException ie) {
                ie.printStackTrace();
            }

            new_resp.waitCompleted();

            logger.debug("ImposterWrapper: returning: " + new_resp);
        }
    }

    static class EventLogger implements EventListener {
        Logger logger;

        public EventLogger(Logger l) {
            logger = l;
        }

        public void connectionOpen(final NHttpConnection conn) {
            logger.info("ImposterWrapper: connection open: " + conn);
        }

        public void connectionTimeout(final NHttpConnection conn) {
            logger.info("ImposterWrapper: connection timed out: " + conn);
        }

        public void connectionClosed(final NHttpConnection conn) {
            logger.info("ImposterWrapper: connection closed: " + conn);
        }

        public void fatalIOException(final IOException ex, final NHttpConnection conn) {
            logger.warn("ImposterWrapper: connection I/O error: " + conn);
        }

        public void fatalProtocolException(final HttpException ex, final NHttpConnection conn) {
            logger.warn("ImposterWrapper: connection HTTP error: " + conn);
        }
    }

    public static void main(String[] args) throws Exception {
        ImposterWrapper iw = new ImposterWrapper("Serf/J 0.0.1");

        HttpRequest req = new BasicHttpRequest("GET", "/");
        req.setHeader("Host", "www.google.com");
        URI u = new URI("http://www.google.com/");

        /*
        HttpRequest req = new BasicHttpRequest("GET", "/static/dojo/demo/demo.html");
        req.setHeader("Host", "localhost:8088");
        URI u = new URI("http://localhost:8088/static/dojo/demo/demo.html");
        */

        HTTPRequestWrapper new_req
            = new HTTPRequestWrapper(req, u, iw);

        iw.requestQueue.put(new_req);

        HTTPResponseWrapper resp = iw.responseQueue.take();

        if (resp.obj != iw) {
            System.out.println("Lost object?");
        }

        HttpEntity entity = resp.resp.getEntity();
        String content = EntityUtils.toString(entity);
        System.out.println("Status: " + resp.resp.getStatusLine());
        System.out.println("Length: " + content.length());
        /*
        System.out.println("---");
        System.out.println(content);
        System.out.println("---");
        */

        resp.completed();
    }
}
