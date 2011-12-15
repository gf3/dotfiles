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

import  java.security.Permission;

import  java.io.PrintStream;

/**
 * Security manager which does nothing other than trap
 * checkExit, or delegate all non-deprecated methods to
 * a base manager.
 * 
 * @author Pete Kirkham
 * 
 */
public class NGSecurityManager extends SecurityManager {
        private static final ThreadLocal EXIT = new InheritableThreadLocal();
        final SecurityManager base;
        
        /**
         * Construct an NGSecurityManager with the given base.
         * @param base the base security manager, or null for no base.
         */
        public NGSecurityManager (SecurityManager base) {
                this.base = base;
        }

        public void checkExit (int status) {
                if (base != null) {
                        base.checkExit(status);
                }
                
                final PrintStream exit = (PrintStream)EXIT.get();
                
                if (exit != null) {
                        exit.println(status);
                }
                
                throw new NGExitException(status);
        }

        public void checkPermission(Permission perm) {
                if (base != null) {
                        base.checkPermission(perm);
                }
        }
   
        public void checkPermission(Permission perm, Object context) {
                if (base != null) {
                        base.checkPermission(perm, context);
                }
        }
        
        public static void setExit (PrintStream exit) {
                EXIT.set(exit);
        }
}
