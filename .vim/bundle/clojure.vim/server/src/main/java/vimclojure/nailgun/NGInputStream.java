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

import java.io.FilterInputStream;
import java.io.IOException;

/**
 * A FilterInputStream that is able to read the chunked stdin stream
 * from a NailGun client.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
class NGInputStream extends FilterInputStream {

	private byte[] header;
	private boolean eof = false;
	private long remaining = 0;
	
	/**
	 * Creates a new NGInputStream wrapping the specified InputStream
	 * @param in the InputStream to wrap
	 */
	public NGInputStream(java.io.InputStream in) {
		super(in);
		header = new byte[5];
	}

	/**
	 * Reads a NailGun chunk header from the underlying InputStream.
	 * 
	 * @throws IOException if thrown by the underlying InputStream,
	 * or if an unexpected NailGun chunk type is encountered.
	 */
	private void readHeader() throws IOException {
		if (eof) return;
		
		int bytesRead = in.read(header);
		int thisPass = 0;
		while (bytesRead < 5) {
			thisPass = in.read(header, bytesRead, 5 - bytesRead);
			if (thisPass < 0) {
				eof = true;
				return;
			}
			bytesRead += thisPass;
		}
		switch(header[4]) {
			case NGConstants.CHUNKTYPE_STDIN:
						remaining = LongUtils.fromArray(header, 0);
						break;
						
			case NGConstants.CHUNKTYPE_STDIN_EOF:
						eof = true;
						break;
						
			default:	throw(new IOException("Unknown stream type: " + (char) header[4]));
		}		
	}
	
	/**
	 * @see java.io.InputStream#available()
	 */
	public int available() throws IOException {
		if (eof) return(0);
		if (remaining > 0) return (in.available());
		return (Math.max(0, in.available() - 5));
	}
	
	/**
	 * @see java.io.InputStream#markSupported()
	 */
	public boolean markSupported() {
		return (false);
	}
	
	/**
	 * @see java.io.InputStream#read()
	 */
	public int read() throws IOException {
		// this should be more readable.
		// this stomps on the first byte of the header array,
		// which is ok
		return((read(header, 0, 1) == -1) ? -1 : (int) header[0]);
	}
	
	/**
	 * @see java.io.InputStream.read(byte[])
	 */
	public int read(byte[] b) throws IOException {
		return (read(b, 0, b.length));
	}
	
	/**
	 * @see java.io.InputStream.read(byte[],offset,length)
	 */
	public int read(byte[] b, int offset, int length) throws IOException {
		if (remaining == 0) readHeader();
		if (eof) return(-1);

		int bytesToRead = Math.min((int) remaining, length);
		int result = in.read(b, offset, bytesToRead);
		remaining -= result;
		return (result);
	}

}
