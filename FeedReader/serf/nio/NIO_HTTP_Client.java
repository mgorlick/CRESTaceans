/*
 * $HeadURL: https://svn.apache.org/repos/asf/httpcomponents/httpcore/tags/4.0.1/httpcore-nio/src/examples/org/apache/http/examples/nio/NHttpClient.java $
 * $Revision: 744516 $
 * $Date: 2009-02-14 17:38:14 +0100 (Sat, 14 Feb 2009) $
 *
 * ====================================================================
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 *
 */
import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.InetSocketAddress;
import java.lang.Throwable; // MG-2009.07.06 Added for debugging.
import java.net.URI;

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


/**
 * Elemental example for executing HTTP requests using the non-blocking I/O model.
 * <p>
 * Please note the purpose of this application is demonstrate the usage of HttpCore APIs.
 * It is NOT intended to demonstrate the most efficient way of building an HTTP client. 
 *
 * 
 * @version $Revision: 744516 $
 */
// class Connection_Event_Logger implements EventListener {

//         public void connectionOpen(final NHttpConnection conn) {
//             System.out.println("Connection open: " + conn);
//         }

//         public void connectionTimeout(final NHttpConnection conn) {
//             System.out.println("Connection timed out: " + conn);
//         }

//         public void connectionClosed(final NHttpConnection conn) {
//             System.out.println("Connection closed: " + conn);
//         }

//         public void fatalIOException(final IOException ex, final NHttpConnection conn) {
//             System.err.println("I/O error: " + ex.getMessage());
//         }

//         public void fatalProtocolException(final HttpException ex, final NHttpConnection conn) {
//             System.err.println("HTTP error: " + ex.getMessage());
//         }
// }

public class NIO_HTTP_Client {
    ConnectingIOReactor io_reactor;
    IOEventDispatch io_event_dispatch;

    //public static HttpHost toHttpHost(Object x) {return (HttpHost)x;} // Coercion procedure for Scheme.

    public static void setTargetHost(final HttpContext context, String key, final Object x) {
	HttpHost target_host = (HttpHost)x;
	//System.err.println("inside setTargetHost");
	context.setAttribute(key, target_host);
	//System.err.println("finished setTargetHost");
    }

//         public void initalizeContext(final HttpContext context, final Object attachment) {
//             HttpHost targetHost = (HttpHost) attachment;
//             context.setAttribute(ExecutionContext.HTTP_TARGET_HOST, targetHost);
//         }


    public NIO_HTTP_Client(
      String user_agent,
      HttpRequestExecutionHandler request_handler,
      EventListener connection_listener) throws Exception {

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
        
	// Allocate an HTTP client handler.
        BufferingHttpClientHandler client_handler = new BufferingHttpClientHandler(
           http_processor, // Basic HTTP Processor.
	   request_handler,
	   new DefaultConnectionReuseStrategy(),
	   parameters);
        client_handler.setEventListener(connection_listener);
        
	// Use two worker threads for the IO reactor.
        io_reactor = new DefaultConnectingIOReactor(2, parameters);
	io_event_dispatch = new DefaultClientIOEventDispatch(client_handler, parameters);

// 	t = new Thread(new Runnable() {
//             public void run() {
//                 try {
//                     io_reactor.execute(io_event_dispatch);
//                 } catch (InterruptedIOException ex) {
//                     System.err.println("Interrupted");
//                 } catch (IOException e) {
//                     System.err.println("I/O error: " + e.getMessage());
// 		    Throwable c = e.getCause();
// 		    if (c != null) {
// 			System.err.println("Cause: " + c.toString());
// 		    }else {
// 			System.err.println("Cause: unknown");
// 		    }
//                 }
//                 System.out.println("Shutdown");
//             }
// 	});
// 	t.start();
    }

    public void go() throws Exception {
	io_reactor.execute(io_event_dispatch); // All exceptions will be propagated to Scheme level.
    }

//     public void go() {
// 	try {
// 	    io_reactor.execute(io_event_dispatch);
// 	} catch (InterruptedIOException ex) {
// 	    System.err.println("Interrupted");
// 	} catch (IOException e) {
// 	    System.err.println("I/O error: " + e.getMessage());
// 	}
// 	System.out.println("Shutdown");
//     }

    // Start an HTTP session with endpoint host:port using the given session handler.
    public SessionRequest openSession(String host, int port, SessionRequestCallback session_handler) {
	return io_reactor.connect(
	  new InetSocketAddress(host, port),
	  null,
	  new HttpHost(host, port), // HTTP host could be different. Change to accomodate for proxy.
	  session_handler);
	}

    public static String uriForRequest (URI u) {
	if (null == u.getQuery()) {
	    return u.getPath();
	} else {
	    int n = u.getPath().length() + u.getQuery().length() + 1;
	    StringBuilder b = new StringBuilder(n);
	    b.append(u.getPath()).append('?').append(u.getQuery());
	    return b.toString();
	}
    }

}


// static class CREST_HTTP_Request_Execution_Handler implements HttpRequestExecutionHandler {

//         private final static String REQUEST_SENT       = "request-sent";
//         private final static String RESPONSE_RECEIVED  = "response-received";
        
//         public CREST_HTTP_Request_Execution_Handler() {
//             super();
//         }
        
//         public void initializeContext(final HttpContext context, final Object attachment) {
//             HttpHost target_host = (HttpHost) attachment;
//             context.setAttribute(ExecutionContext.HTTP_TARGET_HOST, target_host);
//         }
        
//         public void finalizeContext(final HttpContext context) {
// 			// This handler is called when connection closes.
//         }

//         public HttpRequest submitRequest(final HttpContext context) {
//             HttpHost target_host = (HttpHost) context.getAttribute(ExecutionContext.HTTP_TARGET_HOST);
            
                
//                 return new BasicHttpRequest("GET", "/");
//             } else {
//                 // No new request to submit
//                 return null;
//             }
//         }
        
//         public void handleResponse(final HttpResponse response, final HttpContext context) {
//             HttpEntity entity = response.getEntity();
//             try {
//                 String content = EntityUtils.toString(entity);
                
//                 System.out.println("--------------");
//                 System.out.println(response.getStatusLine());
//                 System.out.println("--------------");
//                 System.out.println("Document length: " + content.length());
//                 System.out.println("--------------");
//             } catch (IOException ex) {
//                 System.err.println("I/O error: " + ex.getMessage());
//             }

//             context.setAttribute(RESPONSE_RECEIVED, Boolean.TRUE);
            
//             // Signal completion of the request execution
//             requestCount.countDown();
//         }
        
// }

// class CREST_Session_Request_Callback implements SessionRequestCallback {        
//     public CREST_Session_Request_Callback() {
//     }
        
//     public void cancelled(final SessionRequest request) {
//       System.out.println("Connect request cancelled: " + request.getRemoteAddress());
//     }

//     public void completed(final SessionRequest request) {
//     }

//     public void failed(final SessionRequest request) {
//       System.out.println("Connect request failed: " + request.getRemoteAddress());
//     }

//     public void timeout(final SessionRequest request) {
//       System.out.println("Connect request timed out: " + request.getRemoteAddress());
//      }
// }
    
