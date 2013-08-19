spaceboulders
=============

Asteroids clone in Common Lisp

Tested on SBCL 1.0.57 on Debian


Depends on

lispbuilder-sdl
lispbuilder-sdl-mixer
cl-opengl

Looks for sound files in /sounds- see source code for the ones you will need.  I downloaded some sound-effects from www.freesound.org

Required sound-files

explosion1.wav ping1.wav
explosion2.wav ping2.wav
explosion3.wav ping3.wav
explosion4.wav ping4.wav

music1.mp3
music2.wav
music3.wav

If I can find the CC license attributions I will upload them to make it easier.



Things I wanted to do, but ran out of time in the two weeks train journeys I allowed myself,

Make the UFO change direction move around
Clean up the code (refactor all the state using macros, make it data-driven)
Completely split the sdl/opengl code from the rest of the codebase
Figure out how to make it a package? and make it load the dependencies automatically
Figure out how to make a binary image for various platforms (e.g. build it for Windows)
Figure out why the sound doesn't recover after a REPL abort
Find out how one can connect a REPL to this in live mode, and most infuriatingly of all, figure out how to break the running process :-(


