# VimClojure – a Clojure environment for Vim

VimClojure is one of the most sophisticated editing environments for Clojure.
It provides syntax highlighting, indenting and command completion.

VimClojure is not intended to be an easy to use Clojure IDE, but a plugin
to make life easier for people already familiar with Vim. So you should
be familiar with Vim and/or Java. Eg. VimClojure won't help you in any way
to set up a correct classpath! This is the responsibility of the build
system of the given project. So before using the dynamic server make
yourself comfortable with Clojure, the JVM and Vim.

# Requirements

Please make sure that the following options are set in your .vimrc:

    syntax on
    filetype plugin indent on

Otherwise the filetype is not activated, and hence VimClojure doesn't work.

The following assumes a standard installation. If you have installed Vim (or
are installing VimClojure) in a non-standard way, I trust that you know what
you are doing.

# Online Documentation

Please refer to the online documentation in the doc folder for further
information on how to use VimClojure, its features and its caveats. To
rebuild the help tags for the online documentation issue the following
command in Vim instance.

    " On Unix:
    :helptags ~/.vim/doc
    " On Windows:
    :helptags ~/vimfiles/doc

# Here be Dragons

If requested VimClojure also provides a SLIME like interface to dynamically
work with Clojure code. For this to work the included Nailgun server must be
running. Remote may be forwarded via ssh.

Features of the interactive interface are:

- dynamic documentation lookup
- dynamic javadoc lookup (in an external browser)
- Repl running in a Vim buffer
- smart omni completion
- easy evaluation of code in a buffer

However: **This is not a requirement!** VimClojure works perfectly in
_offline_ mode. That is: just unpack the distribution zip in your .vim
directory and you are good to go! In fact I discourage newbies to use the
server until being more comfortable with Clojure and/or the JVM.

## Configuration

To activate the interactive interface define the vimclojure#WantNailgun variable
in your .vimrc: `let vimclojure#WantNailgun = 1`

## Building the Nailgun interface

You have to download and install the client program once. Unless there is
a note in the release notes it will be compatible with future releases. The
client can be downloaded from:
http://kotka.de/projects/vimclojure/vimclojure-nailgun-client-<version>.zip.

After unzipping the archive, simply type `make` in the subdirectory from the
zip archive. This will compile the nailgun client. For Windows the client is
already pre-compiled as `ng.exe`.

Configure the location of the nailgun client in your `.vimrc`:

    let vimclojure#NailgunClient = "/path/to/your/ng"

It will default to just `ng` which should work if the client is on your PATH.

Note: You might need to check the Makefile for special lib requirements
to compile the nailgun client, eg. on OpenSolaris.

It is **not** required to build the **server side** of the Nailgun interface
and I strongly discourage to do so. The server is provided as jar file
from [Clojars](http://clojars.org). Just add the artifact to your development
dependencies.

* For Gradle (with Clojuresque):
  
      dependencies {
          development 'vimclojure:server:<version>'
      }
  
* For Leiningen:
  
      (defproject …
        :dev-dependencies […
                           [vimclojure/server "<version>"]
                           …])
  
* For Ivy:
  
      <dependency org="vimclojure" name="server" rev="<version>"/>
  
* For Maven:
  
        <dependency>
          <groupId>vimclojure</groupId>
          <artifactId>server</artifactId>
          <version><version></version>
        </dependency>
  

For manual download:
http://clojars.org/repo/vimclojure/server/<version>/server-<version>.jar

There are also launcher scripts included in the vimclojure/bin subdirectory
based on Stephen C. Gilardi's clj-env-dir launcher. See information on how
to use them in the corresponding files.

-- 
Meikel Branmdeyer <mb@kotka.de>
Erlensee, 2010
