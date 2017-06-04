spaceboulders
=============

Asteroids clone in Common Lisp

![Alt text](/screenshot.png)

Tested on SBCL 1.3.18 on Linux Mint 18.1

## Requirements

Spaceboulders can be run as a script or as a compiled binary. The [Common Lisp environment manager Roswell](https://github.com/roswell/roswell) is required to do both.

You'll also need the [SDL 1.2 Development Libraries](https://www.libsdl.org/download-1.2.php).

On ubuntu and its derivatives, look for libsdl1.2-dev, libsdl-gfx1.2-dev, and libsdl-mixer1.2-dev.

Once those dependencies are satisfied, Spaceboulders can be run as a script on the command line like so:

```
$ ./spaceboulders.ros
```

Or it can be compiled and run with the following commands:

```
$ ros build spaceboulders.ros
$ ./spaceboulders
```



Things I wanted to do, but ran out of time in the two weeks train journeys I allowed myself,

Make the UFO change direction move around
Clean up the code (refactor all the state using macros, make it data-driven)
Completely split the sdl/opengl code from the rest of the codebase
Figure out how to make it a package? and make it load the dependencies automatically
Figure out how to make a binary image for various platforms (e.g. build it for Windows and OS X)
Figure out why the sound doesn't recover after a REPL abort
Find out how one can connect a REPL to this in live mode, and most infuriatingly of all, figure out how to break the running process :-(
Enable full-screen - should be no problem but need to add in aspect ratio scaling


All music and sounds taken from  www.freesound.org

CC 3.0 attribution -
   music theremax - Greg Baumont
         jovian 1 & 4 - thatjeffcarter
   sfx   benboncan, herdez, CGEffext, acclivity, lokemon44
