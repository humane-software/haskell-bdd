# What is haskell-bdd?
It is an internal domain-specific language for testing programs using Behavior-Driven Development (BDD) process.

Hackage page: <http://hackage.haskell.org/package/bdd>

# What is BDD?
Behavior-Driven Development is a process of writing tests and documenting software. It comprises of several useful techniques and ideas. You can read more at <http://dannorth.net/introducing-bdd/> and <http://en.wikipedia.org/wiki/Behavior-driven_development>.

# What does this library provide?
This library focuses on the idea of arranging a test in three parts:
  * given (pre-requisites)
  * when (what is being tested)
  * then (what is being asserted)

This mostly is useful for doing higher-perspective tests (such as system test or acceptance tests) in HUnit (as opposed to simple properties in QuickCheck) as it clearly documents what is being tested, what are the pre-conditions and what is asserted. In our experience this is the most useful part of the BDD.

Imagine you want to test a following scenario:

    Given now is 2014-07-15 12:00
    And there are no invoices
    When I issue an invoice
    Then invoice date is 2014-07-15
    And invoice no is 1/2014

Then your test using this library would like like this:

    testThat
    `given_` now "2014-07-15 12:00"
    `given_` noInvoices
    `when_` invoiceIsIssued
    `then_` invoiceDate ^?= "2014-07-15"
    `then_` invoiceNo ^?= "1/2014"

(This, by the way, should be two separate tests, but this is just to show the point).

# What does this library *not* provide?

There are several existing Haskell libraries that provide features that we do not intend to provide:

## Specifying test as plain text
This library is intended for developers, not business people,  and we don't find specifying tests as plain text useful. In fact, this layer of indirection would take away Haskell's great composability. I guess this came with Cucumber and some people I've talked to thought that BDD is all about using "describe" and "it" words and then going on with the usual madness inside the test. I don't think so.

Of course, doing too much code magic in the test can easily go against you but we leave it up to your common sense.

## Assertion combinators
We don't do any assertion combinators, such as "should" and "should not" simply because we found that the standard Haskell functions are as expressive. There is no reason, however, that they should not be used and if you find them useful then please contribute.

# Where is documentation?
There is no proper documentation yet, but please have a look at the library's own tests. They are made of small building blocks that hopefully describe how to use the library. The tests are here: <https://github.com/humane-software/haskell-bdd/blob/master/tests/system/BddTest.hs>.

# What is the status of the library?
We use this library to test several production-grade projects at <http://www.global.de> and <http://humane.software> and it handles pretty much everything we ever wanted form it so far.

If something is not clear or you have some questions, do not hesitate to contact the maintainer and we will try to help you.


