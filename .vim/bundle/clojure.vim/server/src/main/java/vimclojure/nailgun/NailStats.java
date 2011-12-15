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
 * <p>Collects and provides statistics on a nail.</p>
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */

public class NailStats implements Cloneable {

	private Class nailclass;
	private long runCounter;
	private long refCounter;
	private Object lock;
	
	/**
	 * Creates a new NailStats object for the specified class
	 * @param nailclass the class for which we'll collect statistics
	 */
	NailStats(Class nailclass) {
		this.nailclass = nailclass;
		runCounter = 0;
		refCounter = 0;
		lock = new Object();
	}

	/**
	 * Logs the fact that an instance of this nail has started
	 */
	void nailStarted() {
		synchronized(lock) {
			++runCounter;
			++refCounter;
		}
	}
	
	/**
	 * Logs the fact that an instance of this nail has finished
	 */
	void nailFinished() {
		synchronized(lock) {
			--refCounter;
		}
	}

	/**
	 * Returns the number of times this nail has been run.  Nails
	 * that have started but not yet finished are included in this
	 * number.
	 * @return the number of times this nail has been run.
	 */
	public long getRunCount() {
		return (runCounter);
	}
	
	/**
	 * Returns the number of sessions currently running this nail. 
	 * @return the number of sessions currently running this nail. 
	 */
	public long getRefCount() {
		return (refCounter);
	}
	
	/**
	 * Returns the class for which we're tracking statistics
	 * @return the class for which we're tracking statistics
	 */
	public Class getNailClass() {
		return (nailclass);
	}
	
	/**
	 * @see java.lang.Object#hashCode
	 */
	public int hashCode() {
		return (nailclass.hashCode());
	}
	
	/**
	 * Returns true iff the specified <code>NailStats</code> object
	 * is tracking the same class.
	 * @param o the NailStats object to check
	 * @return true iff the specified <code>NailStats</code> object
	 * is tracking the same class.
	 */
	public boolean equals(Object o) {
		NailStats other = (NailStats) o;
		return (nailclass.equals(other.nailclass));
	}
	
	/**
	 * Creates a copy of this <code>NailStats</code> object.
	 * @return a copy of this <code>NailStats</code> object.
	 */
	public Object clone() {
		Object result = null;
		try {
			result = super.clone();
		} catch (CloneNotSupportedException toDiscard) {}
		return (result);
	}
	
	/**
	 * Returns a String representation of this <code>NailStats</code>
	 * object, in the form "classname: runcount/refcount".
	 * *return a String representation of this <code>NailStats</code>
	 * object. 
	 */
	public String toString() {
		return (nailclass.getName() + ": " + getRunCount() + "/" + getRefCount());
	}
}
