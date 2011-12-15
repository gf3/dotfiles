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

import  java.io.PrintStream;

import  org.apache.tools.ant.ExitException;

/**
 * Security exception which wraps an exit status code.
 * @author Pete Kirkham
 */
public class NGExitException extends ExitException {
        public NGExitException(int status) {
                super(status);
        }
  
        /**
         * A lot of code out there, for example ant's Launcher,
         * runs inside a try/catch (Throwable) which will squash
         * this exception; most also calll printStackTrace(), so
         * this re-throws the exception to escape the handling code.
         */
        public void printStackTrace (PrintStream out) {
                throw this;
        }
        
        public void reallyPrintStackTrace (PrintStream out) {
                super.printStackTrace(out);
        }
}
