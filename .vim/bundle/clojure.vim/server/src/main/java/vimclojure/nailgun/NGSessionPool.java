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
 * Provides NGSession pooling functionality.  One parameter, "maxIdle",
 * governs its behavior by setting the maximum number of idle NGSession
 * threads it will allow.  It creates a pool of size maxIdle - 1, because
 * one NGSession is kept "on deck" by the NGServer in order to eke out
 * a little extra responsiveness.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
class NGSessionPool {

	/**
	 * number of sessions to store in the pool
	 */
	int poolSize = 0;

	/**
	 * the pool itself
	 */
	NGSession[] pool = null;
	
	/**
	 * The number of sessions currently in the pool
	 */
	int poolEntries = 0;

	/**
	 * reference to server we're working for
	 */
	NGServer server = null;
	
	/**
	 * have we been shut down?
	 */
	boolean done = false;
	
	/**
	 * synchronization object
	 */
	private Object lock = new Object();
	
	/**
	 * Creates a new NGSessionRunner operating for the specified server, with
	 * the specified number of threads
	 * @param server the server to work for
	 * @param poolSize the maximum number of idle threads to allow
	 */
	NGSessionPool(NGServer server, int maxIdle) {
		this.server = server;
		this.poolSize = maxIdle - 1;
	
		pool = new NGSession[poolSize];
		poolEntries = 0;
	}

	/**
	 * Returns an NGSession from the pool, or creates one if necessary
	 * @return an NGSession ready to work
	 */
	NGSession take() {
		NGSession result;
		synchronized(lock) {
			if (poolEntries == 0) {
				result = new NGSession(this, server);
				result.start();
			} else {
				--poolEntries;
				result = pool[poolEntries];
			}
		}
		return (result);
	}
	
	/**
	 * Returns an NGSession to the pool.  The pool may choose to shutdown
	 * the thread if the pool is full
	 * @param session the NGSession to return to the pool
	 */
	void give(NGSession session) {
		if (done || poolEntries == poolSize) {
			session.shutdown();
		} else {
			synchronized(lock) {
				pool[poolEntries] = session;
				++poolEntries;
			}
		}
	}
	
	/**
	 * Shuts down the pool.  Running nails are allowed to finish.
	 */
	void shutdown() {
		done = true;
		synchronized(lock) {
			while (poolEntries > 0) {
				take().shutdown();
			}
		}
	}

}
