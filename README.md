spaceboulders
=============

Asteroids clone in Common Lisp

Tested on SBCL 1.0.57 on Debian


Depends on

lispbuilder-sdl
lispbuilder-sdl-mixer
cl-opengl

Quickest way to get those installed is by installing quicklisp, and then doing e.g.

(ql:quickload "lispbuilder-sdl")

on each of the dependencies.



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