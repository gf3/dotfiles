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

/**
 * Provides a couple of utility methods that help in reading/writin
 * chunks of data in the Nailgun protocol.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
class LongUtils {

	/**
	 * Encodes the specified long in 4 consecutive bytes (big-endian)
	 * in the specified array, beginning at the specified offset.
	 * @param l the long to encode
	 * @param b the array into which the long should be written
	 * @param offset the offset into the array at which the writing
	 * should begin
	 */
	public static void toArray(long l, byte[] b, int offset) {
		b[offset + 3] = (byte) (l % 256);
		l >>>= 8;
		b[offset + 2] = (byte) (l % 256);
		l >>>= 8;
		b[offset + 1] = (byte) (l % 256);
		l >>>= 8;
		b[0] = (byte) (l % 256);
	}
	
	/**
	 * Reads a 4-byte big-endian long from the specified array,
	 * beginnin at the specified offset.
	 * @param b the array containing the encoded long
	 * @param offset the offset into the array at which the encoded
	 * long begins
	 * @return the decoded long
	 */
	public static long fromArray(byte[] b, int offset) {
		return(((long) (b[offset] & 0xff) << 24)
				+ ((long) (b[offset + 1] & 0xff) << 16)
				+ ((long) (b[offset + 2] & 0xff) << 8)
				+ ((long) (b[offset + 3] & 0xff)));
	}
}
