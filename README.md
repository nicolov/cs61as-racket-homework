# Berkeley CS61AS course homework in Racket

The University of Berkeley offers all the learning material for its awesome CS61AS course through [edX Edge](http://edge.edx.org).

CS61AS is **the** introductory course in Computer Science, based on the world-renowned book *Science and interpretation of computer programs*. 

The name of this course used to be CS61A, but recently the faculty has decided to switch CS61A to Python. They kept the old LISP-based course around and renamed it to CS61AS.

##What's in this repo

In the hope it will be useful to someone, this repo contains the homework for the edX course offered in Fall 2014, implemented in **Racket**. I've found it quite cumbersome to install the Scheme interpreter suggested in the course, and rather preferred to use the Racket IDE.

The code fragments in SICP are written in Simple Scheme, which has a set of simple built-in  operators for list operations and such. For this reason, writing the code in Racket involved leveraging its support for 3rd party *languages* to use some community-contributed bindings. I've used `#lang planet dyoo/simply-scheme:2`. Besides the additional bindings, Racket also somehow diverges from Scheme in the treatment of mutable data structures.

##Resources
It looks like Berkeley is now (Fall 2015) in the process of switching over the course materials to use Racket. You can check their progress [here](http://www.cs61as.org).