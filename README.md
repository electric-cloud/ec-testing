# OVERVIEW:

The ec-testing project is intended to provide a simple testing framework to 
help with your CI/CD process around ElectricCommander.

# USAGE:
invoke ntest with a list of files/directories containing your tests

For example:
   ./ntest --target=ECSERVER:443 examples
   		WIll run all the *.ntest files in the examples subdirectory

# EXAMPLES:
A few examples are provided to get your started with ntest
1. createProject.ntest: a simple example to verofdy a rpoject has been created
	properly
2. createSafeProject.ntest: a few tests to validate a factory procedure called 
	createSaFeProject in the Ntest project. This procedure is a wrapper around 
	the "createProject" API, verifying first that the name contains only 
	letters, numbers and underscores ; and does not start by a number. The
	project Ntest is provided in the Ntest.xml file

# CONTENT:
Here is a brief explanation of the files provided:

* Assert.pm
   assert subroutines for use by ntest.
* ECTest.pm
   Helper routines for running tests against a commander server.
* ntest
   This file contains the basic infrastructure for ElectricCommander tests. It is invoked from the command line to run tests and also defines various routines used by tests, such as the "ntest" subroutine that defines a test, various "assert" subroutines for error checking in tests, and other utilities such as those for making HTTP requests.
* examples
   a library of examples to help you understand the framework.

## MAINTAINER:

Laurent Rochette, Electric Cloud Professional Services Engineer lrochette@electric-cloud.com

## DISCLAIMER:

This module is free for use. Modify it however you see fit to better your experience using ElectricCommander. Share your enhancements and fixes.

This module is not officially supported by Electric Cloud. It has undergone no formal testing and you may run into issues that have not been uncovered in the limited manual testing done so far.

Electric Cloud should not be held liable for any repercusions of using this software.