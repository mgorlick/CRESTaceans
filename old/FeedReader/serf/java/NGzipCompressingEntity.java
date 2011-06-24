/*
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
import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayOutputStream;
import java.util.zip.GZIPOutputStream;

import java.nio.ByteBuffer;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.nio.entity.ProducingNHttpEntity;
import org.apache.http.nio.entity.NHttpEntityWrapper;
import org.apache.http.nio.ContentEncoder;
import org.apache.http.nio.IOControl;
import org.apache.http.message.BasicHeader;
import org.apache.http.protocol.HTTP;

/**
 * Wrapping entity that compresses content when {@link #writeTo writing}.
 *
 * @since 4.0
 */
public class NGzipCompressingEntity extends NHttpEntityWrapper {

    private static final String GZIP_CODEC = "gzip";

    private ByteArrayOutputStream baos;
    private ByteBuffer buffer;

    public NGzipCompressingEntity(final HttpEntity entity) throws IOException {
        super(entity);
        baos = null;
    }

    public Header getContentEncoding() {
        return new BasicHeader(HTTP.CONTENT_ENCODING, GZIP_CODEC);
    }

    public long getContentLength() {
        return -1;
    }

    public boolean isChunked() {
        // force content chunking
        return true;
    }

    public void produceContent(ContentEncoder encoder, IOControl ioctrl)
        throws IOException
    {
        if (baos == null) {
            baos = new ByteArrayOutputStream();
            GZIPOutputStream gzip = new GZIPOutputStream(baos);
            InputStream in = wrappedEntity.getContent();
            byte[] tmp = new byte[2048];
            int l;
            while ((l = in.read(tmp)) != -1) {
                gzip.write(tmp, 0, l);
            }
            gzip.close();

            buffer = ByteBuffer.wrap(baos.toByteArray());
        }

        encoder.write(buffer);
        if(!buffer.hasRemaining())
            encoder.complete();
    }

    public void writeTo(final OutputStream outstream) throws IOException {
        if (outstream == null) {
            throw new IllegalArgumentException("Output stream may not be null");
        }
        System.out.println("Writing gzip");
        GZIPOutputStream gzip = new GZIPOutputStream(outstream);
        InputStream in = wrappedEntity.getContent();
        byte[] tmp = new byte[2048];
        int l;
        while ((l = in.read(tmp)) != -1) {
            gzip.write(tmp, 0, l);
        }
        gzip.close();
    }

} // class GzipCompressingEntity
