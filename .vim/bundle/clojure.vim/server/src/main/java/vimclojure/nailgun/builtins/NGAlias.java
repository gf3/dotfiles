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

import java.util.Iterator;
import java.util.Set;

import vimclojure.nailgun.Alias;
import vimclojure.nailgun.NGContext;
import vimclojure.nailgun.NGServer;

/**
 * <p>Provides a means to view and add aliases.  This is aliased by default
 * to the command "<code>ng-alias</code>".</p>
 * 
 * <p>No command line validation is performed.  If you trigger an exception,
 * your client will display it.</p>
 * 
 * <p><b>To view the current alias list</b>, issue the command:
 * <pre><code>ng-alias</code></pre>
 * with no arguments.</p>
 * 
 * <p><b>To add or replace an alias</b>, issue the command:
 * <pre><code>ng-alias [alias name] [fully qualified aliased class name]</code></pre>
 * </p>
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
public class NGAlias {

	private static String padl(String s, int len) {
		StringBuffer buf = new StringBuffer(s);
		while(buf.length() < len) buf.append(" ");
		return (buf.toString());
	}
	
	public static void nailMain(NGContext context) throws ClassNotFoundException {
		
		String[] args = context.getArgs();
		NGServer server = context.getNGServer();
		
		if (args.length == 0) {
			Set aliases = server.getAliasManager().getAliases();
			
			// let's pad this nicely.  first, find the longest alias
			// name.  then pad the others to that width.
			int maxAliasLength = 0;
			int maxClassnameLength = 0;
			for (Iterator i = aliases.iterator(); i.hasNext();) {
				Alias alias = (Alias) i.next();
				maxAliasLength = Math.max(maxAliasLength, alias.getName().length());
				maxClassnameLength = Math.max(maxClassnameLength, alias.getAliasedClass().getName().length());
			}
			for (Iterator i = aliases.iterator(); i.hasNext();) {
				Alias alias = (Alias) i.next();
				context.out.println(padl(alias.getName(), maxAliasLength) 
										+ "\t" 
										+ padl(alias.getAliasedClass().getName(), maxClassnameLength));
				context.out.println(padl("", maxAliasLength) + "\t" + alias.getDescription());
				context.out.println();
			}
		} else if (args.length == 2) {
			server.getAliasManager().addAlias(new Alias(args[0], "", Class.forName(args[1])));
		}
	}
}
