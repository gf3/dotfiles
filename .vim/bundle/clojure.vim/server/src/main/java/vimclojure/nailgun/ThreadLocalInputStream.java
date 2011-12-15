/*   

  Copyright 2004, Martian Software, Inc.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

*/

package vimclojure.nailgun;

import java.io.IOException;
import java.io.InputStream;

/**
 * The class name is pretty descriptive.  This creates an InputStream
 * much like a FilterInputStream, but with the wrapped InputStream
 * being local to the current Thread.  By setting System.in to a
 * ThreadLocalInputStream, different Threads can read from different
 * InputStreams simply by using System.in.  Of course, the init()
 * method must be called by the Thread that wishes to use the 
 * wrapped stream.
 *  
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
public class ThreadLocalInputStream extends InputStream {

    /**
     * The InputStreams for the various threads
     */
    private InheritableThreadLocal streams = null;

    private InputStream defaultInputStream = null;
    
    /**
     * @param defaultInputStream the InputStream that will be used if the
     * current thread has not called init()
     */
    ThreadLocalInputStream(InputStream defaultInputStream) {
        super();
        streams = new InheritableThreadLocal();
        this.defaultInputStream = defaultInputStream;
        init(null);
    }

    /**
     * Sets the InputStream for the current thread
     * @param streamForCurrentThread the InputStream for the current thread
     */
    public void init(InputStream streamForCurrentThread) {
        streams.set(streamForCurrentThread);
    }

    /**
     * Returns this thread's InputStream
     * @return this thread's InputStream
     */
    public InputStream getInputStream() {
    	InputStream result = (InputStream) streams.get();
    	return ((result == null) ? defaultInputStream : result);
    }

//  BEGIN delegated java.io.InputStream methods

    /**
     * @see java.io.InputStream#available()
     */
    public int available() throws IOException {
        return (getInputStream().available());
    }

    /**
     * @see java.io.InputStream#close()
     */
    public void close() throws IOException {
        getInputStream().close();
    }

    /**
     * @see java.io.InputStream#mark(int)
     */
    public void mark(int readlimit) {
        getInputStream().mark(readlimit);
    }

    /**
     * @see java.io.InputStream#markSupported()
     */
    public boolean markSupported() {
        return (getInputStream().markSupported());
    }

    /**
     * @see java.io.InputStream#read()
     */
    public int read() throws IOException {
        return (getInputStream().read());
    }

    /**
     * @see java.io.InputStream#read(byte[])
     */
    public int read(byte[] b) throws IOException {
        return (getInputStream().read(b));
    }

    /**
     * @see java.io.InputStream#read(byte[],int,int)
     */
    public int read(byte[] b, int off, int len) throws IOException {
        return (getInputStream().read(b, off, len));
    }

    /**
     * @see java.io.InputStream#reset()
     */
    public void reset() throws IOException {
        getInputStream().reset();
    }

    /**
     * @see java.io.InputStream#skip(long)
     */
    public long skip(long n) throws IOException {
        return (getInputStream().skip(n));
    }

//  BEGIN delegated java.io.InputStream methods

//  Note: Should java.lang.Object methods be delegated? If not, and
//  someone synchronizes on this stream, processes might be blocked
//  that shouldn't be.  It would certainly be stupid to delegate
//  finalize().  Not so clear are hashcode(), equals(), notify(), and
//  the wait() methods.
}
