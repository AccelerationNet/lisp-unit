## lisp-unit

*lisp-unit* is a Common Lisp library that supports unit testing. It is
an extension of the [library written by Chris Riesbeck][orig]. There
is a long history of testing packages in Lisp, usually called
"regression" testers. More recent packages in Lisp and other languages
have been inspired by [JUnit for Java][JUnit].

[Documentation is located on the project wiki.][wiki]

## Simplified Interface

The interface for managing tests has been simplified beginning in
Version 0.9.0. The simplified interface is fully described on the
[Reference page][Reference]. The motivation for simplifying the
interface and a comparison with the original interface is described on
the [Simplified Interface page][interface].

[interface]: <https://github.com/OdonataResearchLLC/lisp-unit/wiki/Simplified-Interface>

### How to use lisp-unit

The core definitions of *lisp-unit* may be used by loading the single
file 'lisp-unit.lisp'. To use the extensions, *lisp-unit* must be
loaded using either [Quicklisp][] or [ASDF][].

1. Load (or compile and load) as a single file : `(load "lisp-unit")`.
2. Load using [Quicklisp][] : `(ql:quickload :lisp-unit)`.
3. Load using [ASDF][] : `(asdf:load-system :lisp-unit)`.

### Version 1 Remaining Tasks

* (0.9.0) Test tags to facilitate running test subsets.
* (1.0.0) Expanded internal testing.

### Future Features

* Fixtures
* Test Suites
* Benchmarking tools
* Test Anything Protocol(TAP) support.

[orig]: <http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html>
  "Original Lisp Unit"
[wiki]: <https://github.com/OdonataResearchLLC/lisp-unit/wiki>
  "Lisp Unit Wiki"
[JUnit]: <http://www.junit.org> "JUnit"
[Quicklisp]: <http://www.quicklisp.org> "Quicklisp"
[ASDF]: <http://common-lisp.net/project/asdf/> "ASDF"
