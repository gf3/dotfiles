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

import java.io.InputStream;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Properties;

/**
 * <p>Provides quite a bit of potentially useful information to classes
 * specifically written for NailGun. The <a href="NGServer.html">NailGun server</a> itself, its
 * <a href="AliasManager.html">AliasManager</a>, the remote client's environment variables, and other
 * information is available via this class. For all intents and purposes,
 * the NGContext represents a single connection from a NailGun client.</p>
 * 
 * If a class is written with a
 * 
 * <pre><code>
 * public static void nailMain(NGContext context)
 * </code></pre>
 * 
 * method, that method will be called by NailGun instead of the traditional
 * <code>main(String[])</code> method normally used for programs. A fully populated <code>NGContext</code>
 * object will then be provided to <code>nailMain()</code>.
 * 
 * @author <a href="http://www.martiansoftware.com/contact.html">Marty Lamb </a>
 */
public class NGContext {

	/**
	 * The remote host's environment variables
	 */
	private Properties remoteEnvironment = null;

	/**
	 * The remote host's address
	 */
	private InetAddress remoteHost = null;

	/**
	 * The port on the remote host that is communicating with NailGun
	 */
	private int remotePort = 0;

	/**
	 * Command line arguments for the nail
	 */
	private String[] args = null;

	/**
	 * A stream to which a client exit code can be printed
	 */
	private PrintStream exitStream = null;

	/**
	 * The NGServer that accepted this connection
	 */
	private NGServer server = null;

	/**
	 * The command that was issued for this connection
	 */
	private String command = null;

	private String workingDirectory = null;
	
	/**
	 * The client's stdin
	 */
	public InputStream in = null;

	/**
	 * The client's stdout
	 */
	public PrintStream out = null;

	/**
	 * The client's stderr
	 */
	public PrintStream err = null;

	
	/**
	 * Creates a new, empty NGContext
	 */
	NGContext() {
		super();
	}
	
	void setExitStream(PrintStream exitStream) {
		this.exitStream = exitStream;
	}

	void setPort(int remotePort) {
		this.remotePort = remotePort;
	}

	void setCommand(String command) {
		this.command = command;
	}

	/**
	 * Returns the command that was issued by the client (either an alias or the name of a class).
	 * This allows multiple aliases to point to the same class but result in different behaviors.
	 * @return the command issued by the client
	 */
	public String getCommand() {
		return (command);
	}
	
	void setWorkingDirectory(String workingDirectory) {
		this.workingDirectory = workingDirectory;
	}
	
	/**
	 * Returns the current working directory of the client, as reported by the client.
	 * This is a String that will use the client's <code>File.separator</code> ('/' or '\'),
	 * which may differ from the separator on the server. 
	 * @return the current working directory of the client
	 */
	public String getWorkingDirectory() {
		return (workingDirectory);
	}
	
	void setEnv(Properties remoteEnvironment) {
		this.remoteEnvironment = remoteEnvironment;
	}

	void setInetAddress(InetAddress remoteHost) {
		this.remoteHost = remoteHost;
	}

	void setArgs(String[] args) {
		this.args = args;
	}

	void setNGServer(NGServer server) {
		this.server = server;
	}

	/**
	 * Returns a <code>java.util.Properties</code> object containing a copy
	 * of the client's environment variables
	 * @see java.util.Properties
	 * @return a <code>java.util.Properties</code> object containing a copy
	 * of the client's environment variables
	 */
	public Properties getEnv() {
		return (remoteEnvironment);
	}

	/**
	 * Returns the file separator ('/' or '\\') used by the client's os.
	 * @return the file separator ('/' or '\\') used by the client's os.
	 */
	public String getFileSeparator() {
		return (remoteEnvironment.getProperty("NAILGUN_FILESEPARATOR"));
	}
	
	/**
	 * Returns the path separator (':' or ';') used by the client's os.
	 * @return the path separator (':' or ';') used by the client's os.
	 */
	public String getPathSeparator() {
		return (remoteEnvironment.getProperty("NAILGUN_PATHSEPARATOR"));		
	}
	
	/**
	 * Returns the address of the client at the other side of this connection.
	 * @return the address of the client at the other side of this connection.
	 */
	public InetAddress getInetAddress() {
		return (remoteHost);
	}

	/**
	 * Returns the command line arguments for the command
	 * implementation (nail) on the server.
	 * @return the command line arguments for the command
	 * implementation (nail) on the server.
	 */
	public String[] getArgs() {
		return (args);
	}

	/**
	 * Returns the NGServer that accepted this connection
	 * @return the NGServer that accepted this connection
	 */
	public NGServer getNGServer() {
		return (server);
	}

	/**
	 * Sends an exit command with the specified exit code to
	 * the client.  The client will exit immediately with
	 * the specified exit code; you probably want to return
	 * from nailMain immediately after calling this.
	 * 
	 * @param exitCode the exit code with which the client
	 * should exit
	 */
	public void exit(int exitCode) {
		exitStream.println(exitCode);
	}

	/**
	 * Returns the port on the client connected to the NailGun
	 * server.
	 * @return the port on the client connected to the NailGun
	 * server.
	 */
	public int getPort() {
		return (remotePort);
	}
	
	/**
	 * Throws a <code>java.lang.SecurityException</code> if the client is not
	 * connected via the loopback address.
	 */
	public void assertLoopbackClient() {
		if (!getInetAddress().isLoopbackAddress()) {
			throw (new SecurityException("Client is not at loopback address."));
		}
	}
	
	/**
	 * Throws a <code>java.lang.SecurityException</code> if the client is not
	 * connected from the local machine.
	 */
	public void assertLocalClient() {
		NetworkInterface iface = null;
		try {
			iface = NetworkInterface.getByInetAddress(getInetAddress());
		} catch (java.net.SocketException se) {
			throw (new SecurityException("Unable to determine if client is local.  Assuming he isn't."));
		}
		
		if ((iface == null) && (!getInetAddress().isLoopbackAddress())) {
			throw (new SecurityException("Client is not local."));
		}
	}
}
