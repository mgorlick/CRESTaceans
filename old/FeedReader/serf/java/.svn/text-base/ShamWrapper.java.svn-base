import java.io.File;
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.util.Locale;

import java.net.InetSocketAddress;
import java.net.URI;

import java.util.concurrent.LinkedBlockingQueue;

import org.apache.http.HttpEntity;
import org.apache.http.HttpEntityEnclosingRequest;
import org.apache.http.HttpException;
import org.apache.http.HttpRequest;
import org.apache.http.HttpResponse;
import org.apache.http.HttpResponseInterceptor;
import org.apache.http.HttpStatus;
import org.apache.http.MethodNotSupportedException;
import org.apache.http.impl.DefaultConnectionReuseStrategy;
import org.apache.http.impl.DefaultHttpResponseFactory;
import org.apache.http.impl.nio.DefaultServerIOEventDispatch;
import org.apache.http.impl.nio.reactor.DefaultListeningIOReactor;
import org.apache.http.nio.NHttpConnection;
import org.apache.http.nio.entity.NFileEntity;
import org.apache.http.nio.entity.NStringEntity;
import org.apache.http.nio.protocol.BufferingHttpServiceHandler;
import org.apache.http.nio.protocol.EventListener;
import org.apache.http.nio.reactor.IOEventDispatch;
import org.apache.http.nio.reactor.ListeningIOReactor;
import org.apache.http.params.CoreConnectionPNames;
import org.apache.http.params.CoreProtocolPNames;
import org.apache.http.params.HttpParams;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpRequestHandler;
import org.apache.http.protocol.HttpRequestHandlerRegistry;
import org.apache.http.protocol.BasicHttpProcessor;
import org.apache.http.protocol.ResponseConnControl;
import org.apache.http.protocol.ResponseContent;
import org.apache.http.protocol.ResponseDate;
import org.apache.http.protocol.ResponseServer;
import org.apache.http.util.EntityUtils;

import org.apache.http.protocol.ExecutionContext;

import org.apache.log4j.Logger;

public class ShamWrapper {

    public LinkedBlockingQueue<ShamHandlerWrapper> shamQueue;

    final ListeningIOReactor ioReactor;
    final IOEventDispatch ioEventDispatch;

    Thread ioDispatchThread;
    Thread requestQueueThread;

    Logger logger;

    boolean shuttingDown;

    final String host;
    final int port;
    final String doc_root;
    final String origin_server;

    public ShamWrapper(String _host, int _port, String _doc_root,
                       String _origin_server, Logger l)
        throws Exception
    {
        this.host = _host;
        this.port = _port;
        this.doc_root = _doc_root;
        this.origin_server = _origin_server;
        this.logger = l;

        HttpParams params = new BasicHttpParams();
        params
            .setIntParameter(CoreConnectionPNames.SO_TIMEOUT, 5000)
            .setIntParameter(CoreConnectionPNames.SOCKET_BUFFER_SIZE, 8 * 1024)
            .setBooleanParameter(CoreConnectionPNames.STALE_CONNECTION_CHECK, false)
            .setBooleanParameter(CoreConnectionPNames.TCP_NODELAY, true)
            .setParameter(CoreProtocolPNames.ORIGIN_SERVER, origin_server);

        BasicHttpProcessor httpproc = new BasicHttpProcessor();
        httpproc.addInterceptor(new ResponseGzipCompress());
        httpproc.addInterceptor(new ResponseDate());
        httpproc.addInterceptor(new ResponseServer());
        httpproc.addInterceptor(new ResponseContent());
        httpproc.addInterceptor(new ResponseConnControl());

        BufferingHttpServiceHandler handler = new BufferingHttpServiceHandler(
                httpproc,
                new DefaultHttpResponseFactory(),
                new DefaultConnectionReuseStrategy(),
                params);

        handler.setEventListener(new EventLogger(logger));

        // Set up request handlers
        HttpRequestHandlerRegistry reqistry = new HttpRequestHandlerRegistry();
        reqistry.register("/static/*", new HttpFileHandler(doc_root, logger));
        reqistry.register("*", new ShamHandler());

        handler.setHandlerResolver(reqistry);

        shamQueue = new LinkedBlockingQueue<ShamHandlerWrapper>(10);

        ioReactor = new DefaultListeningIOReactor(2, params);
        ioEventDispatch = new DefaultServerIOEventDispatch(handler, params);

        shuttingDown = false;

        ioDispatchThread = new Thread(new Runnable() {
            public void run() {
                logger.info("ShamWrapper: starting: " + host + ":" + port);
                try {
                    ioReactor.listen(new InetSocketAddress(host, port));
                    ioReactor.execute(ioEventDispatch);
                } catch (InterruptedIOException ex) {
                    logger.warn("ShamWrapper: Interrupted");
                } catch (IOException e) {
                    logger.warn("ShamWrapper: I/O error: " + e.getMessage());
                    e.printStackTrace();
                }
                logger.info("ShamWrapper: I/O Dispatch Thread Shutdown");
            }
        });
        ioDispatchThread.start();
    }

    class ShamHandler implements HttpRequestHandler
    {
        public ShamHandler() {
            super();
        }

        public void handle(final HttpRequest request,
                           final HttpResponse response,
                           final HttpContext context)
            throws HttpException, IOException
        {
            context.setAttribute(ExecutionContext.HTTP_REQUEST, request);

            ShamHandlerWrapper new_sham = new ShamHandlerWrapper(request,
                                                                 response,
                                                                 context);

            logger.debug("ShamWrapper: start put() request: " + new_sham);
            try {
                shamQueue.put(new_sham);
            } catch (java.lang.InterruptedException ie) {
                ie.printStackTrace();
            }

            new_sham.waitCompleted();

            logger.debug("ShamWrapper: end put() request:" + new_sham);
        }
    }

    static class HttpFileHandler implements HttpRequestHandler  {

        private final String docRoot;
        private final Logger logger;

        public HttpFileHandler(final String docRoot, final Logger logger) {
            super();
            this.docRoot = docRoot;
            this.logger = logger;
        }

        public void handle(
                final HttpRequest request,
                final HttpResponse response,
                final HttpContext context) throws HttpException, IOException {
            context.setAttribute(ExecutionContext.HTTP_REQUEST, request);

            String method = request.getRequestLine().getMethod().toUpperCase(Locale.ENGLISH);
            if (!method.equals("GET") && !method.equals("HEAD") && !method.equals("POST")) {
                throw new MethodNotSupportedException(method + " method not supported");
            }

            if (request instanceof HttpEntityEnclosingRequest) {
                HttpEntity entity = ((HttpEntityEnclosingRequest) request).getEntity();
                byte[] entityContent = EntityUtils.toByteArray(entity);
                logger.info("Incoming entity content (bytes): " + entityContent.length);
            }

            String target = request.getRequestLine().getUri();
            final File file = new File(this.docRoot, URLDecoder.decode(target, "UTF-8"));
            if (!file.exists()) {

                response.setStatusCode(HttpStatus.SC_NOT_FOUND);
                NStringEntity entity = new NStringEntity(
                        "<html><body><h1>File" + file.getPath() +
                        " not found</h1></body></html>", "UTF-8");
                entity.setContentType("text/html; charset=UTF-8");
                response.setEntity(entity);
                logger.warn("File " + file.getPath() + " not found");

            } else if (!file.canRead() || file.isDirectory()) {

                response.setStatusCode(HttpStatus.SC_FORBIDDEN);
                NStringEntity entity = new NStringEntity(
                        "<html><body><h1>Access denied</h1></body></html>",
                        "UTF-8");
                entity.setContentType("text/html; charset=UTF-8");
                response.setEntity(entity);
                logger.warn("Cannot read file " + file.getPath());

            } else {

                response.setStatusCode(HttpStatus.SC_OK);
                NFileEntity body = new NFileEntity(file, "text/html");
                response.setEntity(body);
                logger.info("Serving file " + file.getPath());

            }
        }

    }

    static class EventLogger implements EventListener {
        Logger logger;

        public EventLogger(Logger l) {
            logger = l;
        }

        public void connectionOpen(final NHttpConnection conn) {
            logger.info("ShamWrapper: connection open: " + conn);
        }

        public void connectionTimeout(final NHttpConnection conn) {
            logger.info("ShamWrapper: connection timed out: " + conn);
        }

        public void connectionClosed(final NHttpConnection conn) {
            logger.info("ShamWrapper: connection closed: " + conn);
        }

        public void fatalIOException(final IOException ex, final NHttpConnection conn) {
            logger.warn("ShamWrapper: connection I/O error: " + conn);
        }

        public void fatalProtocolException(final HttpException ex, final NHttpConnection conn) {
            logger.warn("ShamWrapper: connection HTTP error: " + conn);
        }
    }

    public static void main(String[] args) throws Exception {
        ShamWrapper sw =
            new ShamWrapper("localhost", 8088,
                            "/Users/jerenkrantz/work/crest/sisc/serf",
                            "Serf/J 0.0.1", Logger.getRootLogger());

        while (true) {
            ShamHandlerWrapper shw = sw.shamQueue.take();
            shw.resp.setEntity(new NStringEntity("Hello world!"));
            shw.completed();
        }
    }
}
