MoonID encryption daemon (slave server)
---------------------------------------

This repository contains all sources necessary to build a template of the
cryptd slave server. The goal is to keep dependencies as minimal as possible
with just one single binary, so it's easy to install and can run on a variety
of different systems/platforms.

How to get the binary
---------------------

The master server is not only responsible for providing a tunnel endpoint but
also for patching/building the slave server binaries with the right default
values and the SSL certificates. If you don't run your own master server, the
vendor will usually provide a location for you where you can download patched
binaries of the slave daemon.

How to install/run
------------------

Just place the downloaded executable in your source folder and execute it prior
to every request you wish to make to the corresponding application residing on
the master side. Default options for your specific application should be
already patched into the binary but can be overridden by a set of commandline
flags:

 * `-l` or `--listenaddress=ADDRESS`
 * `-p` or `--port=PORT`

   Address and port to listen for connections from your application.

 * `--masterhost=ADDRESS`
 * `--masterport=PORT`

   The hostname or IP address and port of the Cryptd master server
   Only useful if you run your own master server.

 * `-u` or `--url=URL`

   The URL where your application handles requests from the the application
   residing on the masters end.

If you don't want the slave server to fork and run in background, you can use
the `-f` (foreground) flag.

To override the instance value of the slave, just provide the new value without
a specific flag.

In case you want to use instances beginning with a dash (`-`), use `--` like
with `-1`:

`cryptd-slave -- -1`

Further options not described here can be shown by running the slave server
with `--help`.
