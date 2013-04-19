IDyl v0.1
---------

This repository holds the code for a Dylan interpreter written in Scheme I wrote back in the mid 90's at the University of Montreal. 

It was written for the Gambit-C compiler as well as the Bigloo compiler. An effort has been done to make the code run using the latest Gambit-C system, but that's not the case for Bigloo.

To build the system, first edit the `Makefile` to fit your system. Then run the following command:

    % make config

at the prompt. Then build the system using the following command:

    % make gambit

Finally, install the compiled executable using the command:

    % make install


To run the interpreter, just run:

    % idyl

Note that the interpreter sometimes behave oddly as only a few tests have been run. Gambit-C has changed considerably in the last 18 years, so it's already amazing that only a few hours of work have been sufficient to make IDyl run again.

Dominique Boucher
April 2013
