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
 * Provides a means to map memorable, short names to classes in order
 * to make the issuing of commands more convenient.  For example, an
 * Alias can map the "<code>mycommand</code>" command to the <code>com.yourdomain.yourpackage.YourClass</code>
 * class.  Obviously, it's a lot easier to type "<code>ng mycommand</code>" than the fully
 * qualified class name.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb</a>
 */
public class Alias implements Comparable {

	/**
	 * The alias name
	 */
	private String name;
	
	/**
	 * The alias description (may be used to provide help to users)
	 */
	private String description;
	
	/**
	 * The class providing a <code>main()</code> or <code>nailMain()</code> method
	 */
	private Class clazz;
	
	/**
	 * Creates a new Alias with the specified properties.
	 * @param name the alias name (short command)
	 * @param description a description of the command
	 * @param clazz the class implementing the command
	 */
	public Alias(String name, String description, Class clazz) {
		if (name == null) throw (new IllegalArgumentException("Alias must have a name."));
		this.name = name.trim();
		if (this.name.length() == 0) throw (new IllegalArgumentException("Alias must have a name."));
		
		if (clazz == null) throw (new IllegalArgumentException("Alias must have an associated class."));
		this.description = description;
		this.clazz = clazz;
	}
	
	/**
	 * Returns the <code>Class</code> object providing a static <code>main()</code> or <code>nailMain()</code> method
	 * for this command.
	 * @return the <code>Class</code> object providing a static <code>main()</code> or <code>nailMain()</code> method
	 * for this command.
	 */
	public Class getAliasedClass() {
		return(clazz);
	}
	
	/**
	 * Returns the name of the aliased command
	 * @return the name of the aliased command
	 */
	public String getName() {
		return (name);
	}
	
	/**
	 * Returns a description for the aliased command
	 * @return a description for the aliased command
	 */
	public String getDescription() {
		return (description);
	}
	
	/**
	 * @see Object#hashCode()
	 */
	public int hashCode() {
		return (name.hashCode());
	}
	
	/**
	 * Checks whether two Aliases have the same name.  Does <b>not</b>
	 * compare any other fields.
	 * @param o the other Alias to check
	 * @return true if the specified Alias has the same name as this Alias.
	 */
	public boolean equals(Object o) {
		return (compareTo(o) == 0);
	}
	
	/**
	 * Compares Alias <b>names</b> - no other fields are compared.
	 * @see Comparable#compareTo(Object)
	 */
	public int compareTo(Object o) {
		return (name.compareTo(((Alias) o).getName()));
	}
}
