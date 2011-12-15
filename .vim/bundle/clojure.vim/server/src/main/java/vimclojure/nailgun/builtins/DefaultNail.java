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

package vimclojure.nailgun.builtins;

import vimclojure.nailgun.NGContext;
import vimclojure.nailgun.NGConstants;

/**
 * The default nail class used by the server when an invalid command (nonexisting
 * classname or alias) is issued.  This simply displays an error message to the
 * client's stdout and has the client exit with value NGConstants.EXIT_NOSUCHCOMMAND.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
public class DefaultNail {

	public static void nailMain(NGContext context) {
		context.err.println("No such command: " + context.getCommand());
		context.exit(NGConstants.EXIT_NOSUCHCOMMAND);
	}
}
