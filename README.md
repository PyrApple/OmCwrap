OM CWRAP PACKAGE
================

Designed for Open Music v. 7.

David Poirier-Quinot, 
IRCAM 09/2016, 
davipoir@ircam.fr

File Content
------------

* Build: Xcode output (amongst which the Cwrap dynamic library)
* cwrap: Opem Muic package folder
* OmCwrap: C library source code 


Install
-------

Copy the cwrap directory to OM package folder. 
Make sure the libOmCwrap.dylib is included in the cwrap directory.

Note on Xcode project creation
------------------------------

Library created from new project, selecting the following options:
OSX / Framework & Library / Library  with STL (C++ Library) option