hsx2hs
========

[![Build Status](https://travis-ci.org/seereason/hsx2hs.svg?branch=master)](https://travis-ci.org/seereason/hsx2hs)
[![Haskell](http://b.repl.ca/v1/language-haskell-4e6272.png)](Http://www.haskell.org)
[![Hackage Status](https://img.shields.io/hackage/v/hsx2hs.svg)][hackage]

[hackage]: https://hackage.haskell.org/package/hsx2hs

The hsx2hs preprocessor translates .hsx source files into ordinary .hs files. Literal
XML syntax is translated into function calls for creating XML values of the appropriate
forms.

hsx2hs transforms literal XML syntax into a series of function calls. Any project
can make use of the syntax by providing definitions for those functions, and the
XML values produced will be of the types specified. This works for any types, since
hsx2hs doesn't make any assumptions, or inserts any information depending on types.

XMLGenerator defines a few typeclasses that together cover the functions injected by the
preprocessor. A project that uses these classes to provide the semantics for the injected
syntax will be able to use any functions written in terms of these, allowing better code
reusability than if each project defines its own semantics for the XML syntax. Also, the classes
makes it possible to use the literal syntax at different types within the same module.
Achieving that is not as simple as it may seem, but the XMLGenerator module provides all the
necessary machinery.

