#!/usr/bin/env perl
# -*-Perl-*-

# Assert module for ntest
#
# Contains assert subroutines for use by ntests.
#
# Copyright (c) 2005-2008 Electric Cloud, Inc.
# All rights reserved

package Assert;

use Carp;
use Exporter;
use FindBin;

@ISA = ('Exporter');

@EXPORT = qw(
              assert
              assertAccessDenied
              assertAttribute
              assertCaseInsensitiveSortedList
              assertCharsEq
              assertCheckboxOrRadio
              assertContains
              assertCssInclude
              assertDate
              assertDef
              assertDiff
              assertEq
              assertFalse
              assertFileDiff
              assertGoodId
              assertHashEq
              assertHeader
              assertHeaderAction
              assertHidden
              assertInList
              assertJavascriptInclude
              assertJavascriptVariable
              assertLabeledCheckbox
              assertMatchingLines
              assertNe
              assertNodeHasPattern
              assertNotInList
              assertNotSettable
              assertNotSubstring
              assertNumericSortedList
              assertOK
              assertPassword
              assertSetEq
              assertSubstring
              assertTableDiagnostic
              assertTextArea
              assertTextInput
              assertTrue
              assertUndef
              assertXHTML
              assertXpath
              errorMessageHeader
              fail
	      skip
              getElementById
              mesg );

use strict;
use warnings;
use Switch;
use XML::Parser::Expat;

$::gDebugLevel = 'd' unless defined($::gDebugLevel);

# ------------------------------------------------------------------------
# isWindows
#
#      Returns true if we're running on Windows.
# ------------------------------------------------------------------------

sub isWindows() {
    return ($^O eq "MSWin32");
}

# ------------------------------------------------------------------------
# mesg
#
#      This subroutine is invoked to print diagnostic information to
#      standard output.  The "level" argument and command-line arguments
#      such as "--debug" are used to determine whether or not to print
#      this message.  If we decide to print this message, then printf
#      is invoked with all of the arguments after "level".
#
# Results:
#      None.
#
# Arguments:
#      level           - (string) indicates what the message refers to:
#                        TEST:    information about a test passing or failing
#                        ERR:     an internal error in the test infrastructure
#                        DBG:     general information about what the test
#                                 infrastructure is doing: used for debugging
#                        TESTDBG: general debugging information provided by
#                                 a specific test
#      message         - (list)   arguments to printf
# ------------------------------------------------------------------------

sub mesg($@) {
    my ($level) = shift;

    if (($level eq 'ERR') ||
            ($level eq 'TEST') ||
            (($level eq 'DBG')  && ($::gDebugLevel =~ /d/)) ||
            (($level eq "TESTDBG") && ($::gDebugLevel =~ /t/))) {
        if ($::gPartialLineOutput) {
            print "\n";
            $::gPartialLineOutput = 0;
        }

        printf(@_);
    }
}

# ------------------------------------------------------------------------
# currentTestLine
#
#      This subroutine computes the file and line number of execution
#      within the current test.
#
# Results:
#      The return value has the form "file:line" for the stack frame
#      just younger than (i.e., invoked by) the most recent "ntest"
#      invocation on the stack.  If there is no such stack frame, then
#      "??:??" is returned.
#
# Arguments:
#      None.
# ------------------------------------------------------------------------

sub currentTestLine() {
    # Search up the call stack to find the frame that invoked "ntest".

    my $pkg;
    my $fileName;
    my $line;
    my $subroutine;

    for (my $i = 3; ($pkg, $fileName, $line, $subroutine) = caller($i);
            $i++) {
        if ($subroutine eq "main::ntest") {
            # This is the frame that invoked ntest; return info about
            # the frame 3 frames lower (i.e., the one that ntest invoked).

            ($pkg, $fileName, $line) = caller($i-3);
            return "$fileName:$line";
        }
    };
    return "??:??";
}

# ------------------------------------------------------------------------
# errorMessageHeader
#
#      This subroutine formats the first line of a message describing a
#      test failure.  By leading this functionality, it's easy to change
#      the formatting to improve readability.
#
# Results:
#      The return value is a string to append to $::gErrorMessages.
#
# Arguments:
#      diagnostic  -     (string) Short description of the assertion failure
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub errorMessageHeader($;$) {
    my ($diagnostic, $desc) = @_;

    $desc = '' unless defined($desc);

    if ($desc) {
        $desc = " ($desc)";
    }
    return "\n---------------------------------------------------------\n"
            . currentTestLine() . ": $diagnostic$desc:"
            . "\n---------------------------------------------------------\n";
}


# ------------------------------------------------------------------------
# assert
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two strings with a given
#      operator and generates an error if they don't match.
#
# Results:
#      If $expected and $actual of the relationship given by $op, then
#      an empty string is returned.  Otherwise (i.e., there was an error)
#      an error message is appended to the global variable $::gErrorMessages
#      and its new value is returned.
#
# Arguments:
#      expectedValue  - (string) Value against which to compare $testValue
#      op              - (string) Operator for comparison: eq, ne, gt, lt,
#                                  ge, le, ==, !=, >, <, >=, <=, or =~.
#      testValue       - (string) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assert($$$;$) {
    my ($expectedValue, $op, $testValue, $desc) = @_;

    # Figure out whether there is an error.

    my $ok;

    if    ($op eq "eq") {$ok = ($expectedValue eq $testValue)}
    elsif ($op eq "ne") {$ok = ($expectedValue ne $testValue)}
    elsif ($op eq "gt") {$ok = ($expectedValue gt $testValue)}
    elsif ($op eq "lt") {$ok = ($expectedValue lt $testValue)}
    elsif ($op eq "ge") {$ok = ($expectedValue ge $testValue)}
    elsif ($op eq "le") {$ok = ($expectedValue le $testValue)}
    elsif ($op eq "==") {$ok = ($expectedValue == $testValue)}
    elsif ($op eq "!=") {$ok = ($expectedValue != $testValue)}
    elsif ($op eq ">")  {$ok = ($expectedValue >  $testValue)}
    elsif ($op eq "<")  {$ok = ($expectedValue <  $testValue)}
    elsif ($op eq ">=") {$ok = ($expectedValue >= $testValue)}
    elsif ($op eq "<=") {$ok = ($expectedValue <= $testValue)}
    elsif ($op eq 'diff') {
        return assertDiff($expectedValue, $testValue, $desc);
    }

    # =~ and !~ are special cases: reverse the arguments for the comparison,
    # since that's the only thing that really makes sense.

    elsif ($op eq "=~") {$ok = ($testValue =~ m/$expectedValue/s)}
    elsif ($op eq "!~") {$ok = ($testValue !~ m/$expectedValue/s)}
    else {
        $::gErrorMessages .= errorMessageHeader("bad assert operator \"$op\"",
                '');
        return avoidExcessiveErrors($::gErrorMessages);
    }

    if ($ok) {
        return;
    }

    # There was an error.  Append nicely formatted message to the test result
    # log.

    $::gErrorMessages .= errorMessageHeader("assert failed", $desc)
            . "----- Expected value was:
$expectedValue
----- Should have been $op this value from test:
$testValue
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# fail
#
#       Invoked by a test to cause an Unconditional test failure.
#
# Results:
#       An error message is appended to the global variable $::gErrorMessages
#       and its new value is returned.
#
# Arguments:
#       message    - (string) Description appended to the error message.
# ------------------------------------------------------------------------

sub fail($) {
    my ($message) = @_;

    $::gErrorMessages .= errorMessageHeader('test error', $message);

    return avoidExcessiveErrors($::gErrorMessages);
}

# -----------------------------------------------------------------------
# skip
#	Invoked by test to skip it (for example for an older version)
#
# Results:
#	A skip message is appended to the global variable $:gSkipMessages
#	and its new value is returned
#
# Arguments:
#       message    - (string) Description appended to the skip message.
# -----------------------------------------------------------------------
sub skip($) {
   my ($message) = @_;

   $::gSkipMessages .= $message;
   return $::gSkipMessages;
}

# ------------------------------------------------------------------------
# assertEq
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two strings and generates an
#      error if they aren't equal.
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue  - (string) Value against which to compare $testValue
#      testValue       - (string) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertEq($$;$) {
    my ($expectedValue, $testValue, $desc) = @_;

    $expectedValue = "UNDEFINED_EXPECTED_VALUE" unless defined($expectedValue);
    $testValue      = "UNDEFINED_TEST_VALUE" unless defined($testValue);
    $desc           = "" unless defined($desc);

    if ($expectedValue eq $testValue) {
        return;
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertEq failed", $desc)
            . "----- Expected value:
$expectedValue
----- Value from test was:
$testValue
-----
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertCharsEq
#
#      This assertion is similar to "assertEq" except that after an
#      error it and does a character-by-character comparison of the
#      two strings to display the differences.
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue   - (string) Value against which to compare $testValue
#      testValue       - (string) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertCharsEq($$;$) {
    my ($expectedValue, $testValue, $desc) = @_;

    if ($expectedValue eq $testValue) {
        return;
    }

    # There was an error.  Figure out which characters are different.

    my $diffs = "";
    my $length1 = length($expectedValue);
    my $length2 = length($testValue);
    my $max = $length1;
    if ($length1 != $length2) {
        $diffs .= sprintf("Lengths different: expected %d, got %d\n",
                $length1, $length2);
        if ($length2 > $max) {
            $max = $length2;
        }
    }
    for (my $i = 0; $i < $max; $i++) {
        my $char1 = ($i < $length1) ? substr($expectedValue, $i, 1) : "";
        my $char2 = ($i < $length2) ? substr($testValue, $i, 1) : "";
        if ($char1 ne $char2) {
            $diffs .= sprintf(
                    "Char %d: expected \"%s\" (%d), got \"%s\" (%d)\n",
                    $i, $char1, ord($char1), $char2, ord($char2));
        }
    }

    $::gErrorMessages .= errorMessageHeader("assertCharsEqfailed", $desc)
            . "----- Expected value:
$expectedValue
----- Value from test was:
$testValue
----- Differences:
$diffs-----
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertDef
#
#     This subroutine s invoked by a test to verify that an object is defined,
#     in the perl sense.
#
# Results:
#     If "actual" is defined, an empty string is returned, otherwise an error
#     message is appended to the global variable $::gErrorMessages and its new
#     value is returned.
#
# Arguments:
#     testValue         - (ref) Value which is verified as defined
#     desc              - (string) Optional description
# ------------------------------------------------------------------------

sub assertDef($;$) {
    my ($testValue, $desc) = @_;

    if (defined($testValue)) {
        return;
    }

    $desc = '' unless defined($desc);

    # The value is not defined.  Append an error.

    $::gErrorMessages .= errorMessageHeader("assertDef failed", $desc);

    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertTrue
#
#     This subroutine is invoked by a test to verify that the testValue
#     evalutes to true, in the perl sense, otherwise an error message
#     is appended to the global variable $::gErrorMessages and its new
#     value is returned.
#
# Arguments:
#     testValue - (scalar) Value which is verified as true.
#     desc      - (string) Optional description
# ------------------------------------------------------------------------

sub assertTrue($;$) {
    my ($testValue, $desc) = @_;

    return if $testValue;

    $desc = '' unless defined($desc);

    # The value is not true.  Append an error.

    $::gErrorMessages .= errorMessageHeader("assertTrue failed", $desc);

    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertUndef
#
#     This subroutine is invoked by a test to verify that an object is not
#     defined, in the perl sense.
#
# Results:
#     If "actual" is not defined, an empty string is returned, otherwise an
#     error message is appended to the global variable $::gErrorMessages and
#     its new value is returned.
#
# Arguments:
#     testValue         - (ref) Value which is verified as defined
#     desc              - (string) Optional description
# ------------------------------------------------------------------------

sub assertUndef($;$) {
    my ($testValue, $desc) = @_;

    if (!defined($testValue)) {
        return;
    }

    $desc = '' unless defined($desc);

    # The value is not defined.  Append an error.

    $::gErrorMessages .= errorMessageHeader("assertUndef failed", $desc);

    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertNe
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two strings and generates an
#      error if they are equal.
#
# Results:
#      If "expected" and "actual" are "ne", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue  - (string) Value against which to compare $testValue
#      testValue       - (string) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertNe($$;$) {
    my ($expectedValue, $testValue, $desc) = @_;

    $testValue = "#UNDEF" unless defined($testValue);
    $expectedValue = "#UNDEF" unless defined($expectedValue);

    if ($expectedValue ne $testValue) {
        return;
    }

    $desc = '' unless defined($desc);

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertNe failed", $desc)
            . "----- Expected value should be different from:
$expectedValue
----- Value from test was:
$testValue
-----
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertGoodId
#
#     This subroutine is invoked by a test to verify that an object is
#     a valid object-id (e.g. job-id, job-step-id, etc.)  It actually
#     calls a few other assert* routines to do the work.
#
# Results:
#     If "testValue" is a valid id, an empty string is returned, otherwise an
#     error message is appended to the global variable $::gErrorMessages and
#     its new value is returned.
#
# Arguments:
#     testValue         - (ref) Value which is verified as being a good id.
#     desc              - (string) Optional description
# ------------------------------------------------------------------------

sub assertGoodId($;$) {
    my ($testValue, $desc) = @_;

    $desc = "" unless defined($desc);

    assertDef($testValue, "defined: $desc");
    if (!defined($testValue)) {
        return avoidExcessiveErrors($::gErrorMessages);
    }

    assertNe("", $testValue, "not blank: $desc");
    if ($testValue eq "") {
        return avoidExcessiveErrors($::gErrorMessages);
    }

    # Check if jobId is UUID
    if ($testValue =~ "[0-9a-f]{8}(?:-[0-9a-f]{4}){4}[0-9a-f]{8}") {
        return "";
    }

    return assert(0, '<', int($testValue), "positive int: $desc");
}

# ------------------------------------------------------------------------
# assertFalse
#
#     This subroutine is invoked by a test to verify that the testValue
#     evalutes to false, in the perl sense, otherwise an error message
#     is appended to the global variable $::gErrorMessages and its new
#     value is returned.
#
# Arguments:
#     testValue - (scalar) Value which is verified as false.
#     desc      - (string) Optional description
# ------------------------------------------------------------------------

sub assertFalse($;$) {
    my ($testValue, $desc) = @_;

    return if !$testValue;

    $desc = '' unless defined($desc);

    # The value is not false.  Append an error.

    $::gErrorMessages .= errorMessageHeader("assertFalse failed", $desc);

    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertHashEq
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two hashes and generates an
#      error if they aren't equivalent.  Orphans in either hash and
#      value differences for common keys are reported.
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue  - (hash ref) Value against which to compare $testValue
#      testValue       - (hash ref) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertHashEq {
    my ($hashRef1, $hashRef2, $desc) = @_;

    my $diffs = '';

    foreach (keys (%$hashRef1)) {
        if (exists($hashRef2->{$_})) {
            my $expectedValue = $hashRef1->{$_} || "#UNDEF";
            my $testValue = $hashRef2->{$_} || "#UNDEF";
            if (ref($expectedValue) eq "HASH" && ref($testValue) eq "HASH") {
                assertHashEq($expectedValue, $testValue, "$desc -> $_");
            } elsif (ref($expectedValue) eq "ARRAY"
                     && ref($testValue) eq "ARRAY") {
                assertSetEq($expectedValue, $testValue, "$desc -> $_");
            } elsif ($expectedValue ne $testValue) {
                $diffs .= "$_ different:\n   expected: \"$expectedValue\"\n"
                        . "  test: \"$testValue\"\n";
            }
        } else {
            # Key doesn't exist in hash2.
            $diffs .= "$_ only in expected.\n";
        }
    }

    # Now find any elements in hash2 that aren't in hash1.
    foreach (keys (%$hashRef2)) {
        if (!exists($hashRef1->{$_})) {
            $diffs .= "$_ only in actual.\n";
        }
    }

    if ($diffs eq '') {
        return;
    } else {
        $::gErrorMessages .= errorMessageHeader("assertHashEq failed", $desc)
                . "----- Hash diffs:
$diffs
";
        return avoidExcessiveErrors($::gErrorMessages);
    }
}

# ------------------------------------------------------------------------
# assertSetEq
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two sets (arrays) and generates an
#      error if they aren't equivalent.  Orphans in either list
#      are reported.
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue  - (array ref) Value against which to compare $testValue
#      testValue       - (array ref) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertSetEq($$;$) {
    my ($arrayRef1, $arrayRef2, $desc) = @_;

    # Convert the arrays to hashes and then do an assertHashEq on that.
    my %hash1 = ();
    foreach my $val (@$arrayRef1) {
        $hash1{$val} = 1;
    }

    my %hash2 = ();
    foreach my $val (@$arrayRef2) {
        $hash2{$val} = 1;
    }

    return assertHashEq(\%hash1, \%hash2, $desc);
}

# ------------------------------------------------------------------------
# assertDate
#
#       Invoked by a test to compare two date/time strings.
#
# Results:
#       If "expected" and "actual" compare using the supplied operator, then an
#       empty string is returned.  Otherwise (i.e., there was an error) an
#       error message is appended to the global variable $::gErrorMessages and
#       its new value is returned.
#
# Arguments
#       expectedValue   - (string) ISO8601 datetime string against which to
#                                  compare $testValue
#       op              - (string) comparison operator, <, <=, >, >=, ==
#       testValue       - (string) ISO8601 datetime string generated by test
#       desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertDate($$$;$) {
    my ($expectedValue, $op, $testValue, $desc) = @_;

    $desc = '' unless defined($desc);

    # Convert strings to date objects

    my $expected = DateTime::Format::ISO8601->parse_datetime($expectedValue);
    my $test = DateTime::Format::ISO8601->parse_datetime($testValue);

    # Formatter doesn't support milliseconds, so add them in explicitly

    $expectedValue =~ /.+\.([0-9]?[0-9]?[0-9])$/;
    my $expectedMillis = $1;
    $testValue =~ /.+\.([0-9]?[0-9]?[0-9])$/;
    my $testMillis = $1;

    if (defined($expectedMillis)) {
        $expected->set_nanosecond($expectedMillis * 1000000);
    }
    if (defined($testMillis)) {
        $test->set_nanosecond($testMillis * 1000000);
    }

    my $ok;

    if    ($op eq "<")  {$ok = ($expected <  $test)}
    elsif ($op eq ">")  {$ok = ($expected >  $test)}
    elsif ($op eq "==") {$ok = ($expected == $test)}
    elsif ($op eq "<=") {$ok = ($expected <= $test)}
    elsif ($op eq ">=") {$ok = ($expected >= $test)}
    else {
        $::gErrorMessages .=
            errorMessageHeader("bad assert operation \"$op\"", '');
        return avoidExcessiveErrors($::gErrorMessages);
    }

    if ($ok) {
        return;
    }

    # There was an error.  Append nicely formatted message to the test result
    # log.

    $::gErrorMessages .= errorMessageHeader("assert failed", $desc)
        . "----- Expected value was:
$expectedValue (date: $expected)
----- Should have been $op this value from test:
$testValue (date: $test)
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertDiff
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two strings and generates an
#      error if they aren't equal.  Furthermore, if there is an error,
#      we use 'diff' output to indicate where the differences are.
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue   - (string) Value against which to compare $testValue
#      testValue       - (string) Value generated by test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertDiff($$;$) {
    my ($expectedValue, $testValue, $desc) = @_;

    if ($expectedValue eq $testValue) {
        return;
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    open(DIFF, "> $::gTempDir/ntest.diffassert.0");
    print(DIFF "$expectedValue");
    close(DIFF);
    open(DIFF, "> $::gTempDir/ntest.diffassert.1");
    print(DIFF "$testValue");
    close(DIFF);
    my $diffs =
        `diff $::gTempDir/ntest.diffassert.0 $::gTempDir/ntest.diffassert.1`;
    unlink "$::gTempDir/ntest.diffassert.0", "$::gTempDir/ntest.diffassert.1";

    $::gErrorMessages .= errorMessageHeader("assertDiff failed", $desc)
            . "----- Expected value:
$expectedValue
----- Value from test was:
$testValue
----- Differences:
$diffs
-----
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertFileDiff
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It compares two files with diff and generates an
#      error if they aren't equal. It is assumed that the file
#      diff may be too big to display or may be binary, so they are not shown
#
# Results:
#      If "file1" and "file2" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      file1       - (string) first file (full path)
#      file2       - (string) second file (full path)
#      desc        - (string) Optional description
# ------------------------------------------------------------------------

sub assertFileDiff($$;$) {
    my ($file1, $file2, $desc) = @_;

    # ignore diffs in line endings so files can be from diff platforms
    my $diffs = `diff --strip-trailing-cr "$file1" "$file2"`;

    if ($? == 0  and  length($diffs) == 0) {
        return;
    }

    $::gErrorMessages .= errorMessageHeader("assertFileDiff failed", $desc)
            . "----- Expected value:
file contents are the same
----- Value from test was:
$file1
  contents differ from
$file2
----- First 25 lines of diff
";
    my @lines=  split /^/, $diffs, 100 ;
    my $count = 25;
    foreach my $line (@lines) {
        $::gErrorMessages .= $line;
        $count -= 1;
        if ($count < 0) {
            last;
        }
    }
    return avoidExcessiveErrors($::gErrorMessages);
}



# ------------------------------------------------------------------------
# assertXHTML
#
#      Verify that a given document is valid XHTML.
#
# Results:
#      If "contents" contain valid XHTML, then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      contents     - (string) XML document to validate
#      desc         - (string) Optional description
# ------------------------------------------------------------------------

sub assertXHTML($;$) {

    mesg('DBG', "assertXHTML\n");

    my ($contents, $desc) = @_;
    my $f;
    # Write out the contents to a tempfile.

    mesg('DBG', "assertXHTML : writing contents to ntest-xmlvalid.tmp\n");
    open ($f, "> ntest-xmlvalid.tmp");
    binmode $f, ':utf8';
    print $f $contents;
    close ($f);

    my $output;

    my $command = "which xmlvalid";
    if (!isWindows()) {
        $command .= " 2>/dev/null";
    }


    mesg('DBG', "assertXHTML : writing catalog\n");
    open (TEMP, "> ntest-xmlvalid-catalog.tmp");
    print TEMP qq{
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
<rewriteSystem systemIdStartString="http://www.w3.org/TR/xhtml1/DTD/"
      rewritePrefix="file:///$::gToolsDir/common/share/xhtml/"/>

</catalog>};
    close (TEMP);

    my $xmlvalid_is_installed_in_our_path = `$command`;
    chomp $xmlvalid_is_installed_in_our_path;
    if (-e $xmlvalid_is_installed_in_our_path) {
        # Run ElCel 'xmlvalid' to check validity.

        mesg('DBG', "assertXHTML : running xmlvalid\n");
        my $dtd = "$::gToolsDir/common/share/xhtml/xhtml1-strict.dtd";
        $output = `xmlvalid -q "-g ntest-xmlvalid-catalog.tmp" "--dtd=$dtd" ntest-xmlvalid.tmp`;
    } else {
        # No xmlvalid on this platform, so just verify that the document
        # is well formed.

        my $p = new XML::Parser::Expat;
        eval {$p->parsefile("ntest-xmlvalid.tmp")};
        $output = $@;
    }

    unlink("ntest-xmlvalid.tmp");
    unlink("ntest-xmlvalid-catalog.tmp");
    if (!$output) {
        return;
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    chomp $output;
    chomp $contents;
    $::gErrorMessages .= errorMessageHeader("assertXHTML failed", $desc)
            . "----- Bad XHTML:
$contents
----- validation errors:
$output
";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertInList
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that at least one
#      of the entries in a list has a particular value.
#
# Results:
#      If "desiredValue" is present as an element in "list" (exact
#      matching), then an empty string is returned.  Otherwise (i.e.,
#      there was an error) an error message is appended to the global
#      variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      desiredValue    - (string) Value to search for in $list
#      list            - (string) List of strings that came from the test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertInList($$;$) {
    my ($desiredValue, $list, $desc) = @_;

    foreach my $element (@$list) {
        if ($desiredValue eq $element) {
            return '';
        }
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertInList failed", $desc)
            . "----- Value that should have appeared in list:
$desiredValue
----- List of values from test:\n";
    foreach my $element (@$list) {
        $::gErrorMessages .= $element . "\n";
    }
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertNotInList
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that none
#      of the entries in a list has a particular value.
#
# Results:
#      If "desiredValue" is not present as an element in "list" (exact
#      matching), then an empty string is returned.  Otherwise (i.e.,
#      there was an error) an error message is appended to the global
#      variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      desiredValue    - (string) Value to search for in $list
#      list            - (string) List of strings that came from the test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertNotInList($$;$) {
    my ($desiredValue, $list, $desc) = @_;
    my $found = 0;
    foreach my $element (@$list) {
        if ($desiredValue eq $element) {
            # There was an error.  Append nicely formatted message to the
            # test result log.

            $::gErrorMessages .=
                errorMessageHeader("assertNotInList failed", $desc)
                . "----- Value that should not have appeared in list:
$desiredValue
----- List of values from test:\n";
            foreach my $element (@$list) {
                $::gErrorMessages .= $element . "\n";
            }
            return avoidExcessiveErrors($::gErrorMessages);
        }
    }

    # If we get here, the element wasn't found.  Yay!
    return '';
}

# ------------------------------------------------------------------------
# assertCaseInsensitiveSortedList
#
#      This subroutine is invoked by a test to verify that the test performed
#      correctly.  It checks to be sure that the given list is sorted (case
#      insensitively).
#
# Results:
#      If "list" is sorted, then an empty string is returned.  Otherwise
#      (i.e., there was an error) an error message is appended to the global
#      variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      list            - (list ref) List of strings that came from the test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertCaseInsensitiveSortedList($;$) {
    my ($list, $desc) = @_;

    # Ok, some people's definition of case-insensitive sorting varies from
    # others'.
    #
    # In SQL Server, '_' is a word separator.  In MySQL, '_' is after all
    # letters.  This translates to two different ways to do case-insensitive
    # comparisons in a sort function.  Also, SQL Server sorts space before
    # '_'.

    my $dbtype = defined($ENV{"COMMANDER_DB_TYPE"})
        ? $ENV{"COMMANDER_DB_TYPE"} : 'mysql';
    my $actualListStr = join("\n", @$list);
    my $sortedListStr;

    if ($dbtype eq 'sqlserver') {
        my @sqlServerSortedList = sort {

            # Split the string along word boundaries, then compare each string
            # fragment individually.  We include the pattern in the array,
            # because it is significant.
            my @left    = split(/([_ ])/, $a);
            my @right   = split(/([_ ])/, $b);
            my $minSize = @left < @right ? @left : @right;

            for (my $ind = 0; $ind < $minSize; $ind++) {
                my $res = lc($left[$ind]) cmp lc($right[$ind]);

                if ($res != 0) {
                    # We found a diff!
                    return $res;
                }
            }

            # No diff found yet, so one is basically a substring of the other.
            # Return -1 if left is smaller than right, else 1.

            return @left == $minSize ? -1 : 1;
        } @$list;

        $sortedListStr = join("\n", @sqlServerSortedList);
    } elsif ($dbtype eq 'db2' || $dbtype eq 'oracle') {
        # db2 is case-sensitive
        my @sortedList = sort {$a cmp $b} @$list;

        $sortedListStr = join("\n", @sortedList);
    } else {
        my @ucSortedList = sort {uc($a) cmp uc($b)} @$list;

        $sortedListStr = join("\n", @ucSortedList);
    }

    return assertDiff($sortedListStr, $actualListStr, $desc);
}

# ------------------------------------------------------------------------
# assertNumericSortedList
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that the given list
#      is sorted.
#
# Results:
#      If "list" is sorted, then an empty string is returned.  Otherwise (i.e.,
#      there was an error) an error message is appended to the global
#      variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      list            - (list ref) List of strings that came from the test
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertNumericSortedList($;$) {
    my ($list, $desc) = @_;

    my @sortedList = sort {$a <=> $b} @$list;
    assertEq(join(",", @sortedList), join(",", @$list), $desc);
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertSubstring
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that a expected
#      string is contained as a substring of a test result.
#
# Results:
#      If $substring exists as a substring of $string (exact matching),
#      then an empty string is returned.  Otherwise (i.e., there was
#      an error) an error message is appended to the global variable
#      $::gErrorMessages and its new value is returned.
#
# Arguments:
#      substring       - (string) Text that should appear somewhere
#                        within $string
#      string          - (string) String in which to search for $substring
#                        (most likely, output from a test)
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertSubstring($$;$) {
    my ($substring, $string, $desc) = @_;

    if (index($string, $substring) >= 0) {
        return '';
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertSubstring failed", $desc)
            . "----- Expected substring:
$substring
----- Not present in test output:
$string\n";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertNotSubstring
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that a expected
#      string is not contained as a substring of a test result.
#
# Results:
#      If $substring doesn't exist as a substring of $string (exact matching),
#      then an empty string is returned.  Otherwise (i.e., there was an error)
#      an error message is appended to the global variable $::gErrorMessages
#      and its new value is returned.
#
# Arguments:
#      substring       - (string) Text that should not appear anywhere
#                        within $string
#      string          - (string) String in which to search for $substring
#                        (most likely, output from a test)
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertNotSubstring($$;$) {
    my ($substring, $string, $desc) = @_;

    if (index($string, $substring) < 0) {
        return '';
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertSubstring failed", $desc)
            . "----- Expected substring:
$substring
----- Present in test output:
$string\n";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertContains
#
#      (deprecated cause of backward syntax: use "assertSubstring" instead)
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It checks to be sure that the output
#      from a test contains a particular string.
#
# Results:
#      If $substring exists as a substring of $string (exact matching),
#      then an empty string is returned.  Otherwise (i.e., there was
#      an error) an error message is appended to the global variable
#      $::gErrorMessages and its new value is returned.
#
# Arguments:
#      string          - (string) String in which to search for $substring
#                        (most likely, output from a test)
#      substring       - (string) Text that should appear somewhere
#                        within $string
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertContains($$;$) {
    my ($string, $substring, $desc) = @_;

    if (index($string, $substring) >= 0) {
        return '';
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertContains failed", $desc)
            . "----- Expected substring:
$substring
----- Not present in test output:
$string\n";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertMatchingLines
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It extracts from the test output all of
#      the lines matching a given pattern, then compares the results
#      to a expected string.
#
# Results:
#      If $expected equals the set of lines from $string matching the
#      regular expression pattern $pattern, then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expected       - (string) Expected result when matching lines
#                        are extracted from $string.
#      string          - (string) String from which to extract lines
#                        (most likely, output from a test).
#      pattern         - (string) Extract all lines matching this pattern.
#      desc            - (string) Optional description
# ------------------------------------------------------------------------

sub assertMatchingLines($$$;$) {
    my ($expected, $string, $pattern, $desc) = @_;

    my $lines = '';
    foreach my $line (split("\n", $string)) {
        if ($line =~ m/$pattern/) {
            $lines .= $line . "\n";
        }
    }
    if ($lines eq $expected) {
        return '';
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("assertMatchingLines failed",
            $desc) . "----- Expected string:
$expected
----- Did not equal lines matching |$pattern|:
$lines\n";
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertNodeHasPattern
#
#      This subroutine verifies that a given text pattern appears
#      in the combined text of a node and all its descendents.
#
# Results:
#      An empty string is returned if $pattern matches the text of $node.
#      Otherwise an error message is appended to the global variable
#      $::gErrorMessages.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  node.
#      node                      - (XML::XPath::Node) Node to search.
#      pattern                   - (string) Regular expression describing
#                                  the desired text.
#      desc                      - (string) Display this in the error
#                                  message if the pattern isn't found.
# ------------------------------------------------------------------------

sub assertNodeHasPattern($$$;$) {
    my ($xpath, $node, $pattern, $desc) = @_;
    my $text = $xpath->findvalue('.', $node);
    if ($text =~ m/$pattern/s) {
        return '';
    }
    if ($pattern eq '') {
        return;
    }
    $::gErrorMessages .= errorMessageHeader("assertNodeHasPattern failed",
            $desc) . ::outdent("
           |----- Pattern:
            \"$pattern\"
            ----- Didn't match text of node:
            $text\n");
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertAttribute
#
#      This subroutine verifies that an attribute of a given node
#      has a particular value.
#
# Results:
#      An empty string is returned if the specified attribute has the
#      specified value.  Otherwise an error message is appended to the
#      global variable $::gErrorMessages and the new value of that variable
#      is returned.
#
# Arguments:
#      node                      - (XML::XPath::Node) Node to search.
#      attribute                 - (string) Name of the attribute to check.
#      value                     - (string) Expected value for the
#                                  attribute.  Empty string means there
#                                  should be no value.
#      desc                      - (string) Display this in the error
#                                  message if the pattern isn't found.
# ------------------------------------------------------------------------

sub assertAttribute($$$;$) {
    my ($node, $attribute, $value, $desc) = @_;

    my $actualValue = $node->getAttribute($attribute);
    if ($value) {
        if (!$actualValue) {
            $::gErrorMessages .= errorMessageHeader(
                    "assertAttribute failed", $desc)
                    . ::outdent("
                   |----- No \"$attribute\" attribute present, expected this:
                    $attribute=\"$value\"\n");
            return avoidExcessiveErrors($::gErrorMessages);
        } elsif ($actualValue ne $value) {
            $::gErrorMessages .= errorMessageHeader(
                    "assertAttribute failed", $desc)
                    . ::outdent("
                   |----- Expected this attribute:
                    $attribute=\"$value\"
                    ----- Actual attribute was this:
                    $attribute=\"$actualValue\"\n");
            return avoidExcessiveErrors($::gErrorMessages);
        }
    } else {
        if ($actualValue) {
            $::gErrorMessages .= errorMessageHeader(
                    "assertAttribute failed", $desc)
                    . ::outdent("
                   |----- Expected no \"$attribute\" attribute, found this:
                    $attribute=\"$actualValue\"\n");
            return avoidExcessiveErrors($::gErrorMessages);
        }
    }
    return '';
}

# ------------------------------------------------------------------------
# getElementById
#
#      This subroutine verifies the existence in an HTML document of
#      element with a given "id" attribute.
#
# Results:
#      If there exists exactly one element with id $id, it is returned.
#      Otherwise an error message is appended to the global variable
#      $::gErrorMessages and undef is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      id                        - (string) Look for an element with an
#                                  "id" attribute with this value.
#      desc                      - (string) Display this in the error
#                                  message if the element doesn't exist.
# ------------------------------------------------------------------------

sub getElementById($$$) {
    my ($xpath, $id, $desc) = @_;
    my $nodes = $xpath->findnodes('//*[@id="' . $id . '"]');
    if ($nodes->size > 0) {
        return $nodes->get_node(0);
    }

    # There was an error.  Append nicely formatted message to the
    # test result log.

    $::gErrorMessages .= errorMessageHeader("getElementById failed", $desc)
            . "----- No element with id \"$id\"\n";
    return undef;
}

# ------------------------------------------------------------------------
# assertTextInput
#
#      This subroutine verifies the existence in an HTML document of
#      a text input with a given label and (possibly) an initial value.
#
# Results:
#      If there exists an element with id $id matching $label, $name,
#      and $value, then an empty string is returned.  Otherwise (i.e.,
#      there was an error) one or more error messages are appended to
#      the global variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      id                        - (string) id of an element containing
#                                  the input and its label.
#      pattern                   - (string) Regular expression that should
#                                  match the text contained within the
#                                  element given by $id.
#      name                      - (string) Expected "name" attribute
#                                  for an input element within $id.
#      value                     - (string) Expected value for the
#                                  element.  Empty string means there
#                                  should be no value.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertTextInput($$$$$;$) {
    my ($xpath, $id, $pattern, $name, $value, $desc) = @_;
    my $result = '';
    my $element = getElementById($xpath, $id, $desc);
    if (!$element) {
        # Desired element doesn't exist.

        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertNodeHasPattern($xpath, $element, $pattern, $desc)) {
        $result = $::gErrorMessages;
    }
    my $nodes = $xpath->findnodes('.//input[@type="text"]'
            . '[@name="' . $name . '"]', $element);
    if ($nodes->size == 0) {
        $::gErrorMessages .= errorMessageHeader("assertTextInput failed",
                $desc)
                . "----- Couldn't find text input with name=\"$name\"\n";
        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertAttribute($nodes->get_node(0), "value", $value, $desc)) {
        $result = $::gErrorMessages;
    }
    return $result;
}

# ------------------------------------------------------------------------
# assertPassword
#
#      This subroutine verifies the existence in an HTML document of
#      a password input with a given label and (possibly) an initial value.
#
# Results:
#      If there exists an element with id $id matching $label, $name,
#      and $value, then an empty string is returned.  Otherwise (i.e.,
#      there was an error) one or more error messages are appended to
#      the global variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      id                        - (string) id of an element containing
#                                  the password and its label.
#      pattern                   - (string) Regular expression that should
#                                  match the text contained within the
#                                  element given by $id.
#      name                      - (string) Expected "name" attribute
#                                  for a password input element within $id.
#      value                     - (string) Expected value for the
#                                  password.  Empty string means there
#                                  should be no value.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertPassword($$$$$;$) {
    my ($xpath, $id, $pattern, $name, $value, $desc) = @_;
    my $result = '';
    my $element = getElementById($xpath, $id, $desc);
    if (!$element) {
        # Desired element doesn't exist.

        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertNodeHasPattern($xpath, $element, $pattern, $desc)) {
        $result = $::gErrorMessages;
    }
    my $nodes = $xpath->findnodes('.//input[@type="password"]'
            . '[@name="' . $name . '"]', $element);
    if ($nodes->size == 0) {
        $::gErrorMessages .= errorMessageHeader("assertPassword failed", $desc)
                . "----- Couldn't find password input with name=\"$name\"\n";
        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertAttribute($nodes->get_node(0), "value", $value, $desc)) {
        $result = $::gErrorMessages;
    }
    return $result;
}

# ------------------------------------------------------------------------
# assertCheckboxOrRadio
#
#      This subroutine verifies the existence in an HTML document of
#      a checkbox or radiobutton with a textual label and (possibly)
#      an initial state of "checked".
#
# Results:
#      If there exists a checkbutton or radiobutton that matches $id,
#      $label, $name, and $checked, then an empty string is returned.
#      Otherwise (i.e., there was an error) one or more error messages
#      are appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      container                 - (string) id of an element containing
#                                  the button and its label.
#      pattern                   - (string) Regular expression that should
#                                  match the text contained within the
#                                  element given by $container.
#      type                      - (string) Type of button: "radio" or
#                                  "checkbox"
#      name                      - (string) Expected "name" attribute
#                                  for a checkbox or radio element in
#                                  $container.
#      id                        - (string) Expected "id" attribute
#                                  for a checkbox or radio element in
#                                  $container.
#      value                     - (string) Expected value attribute for
#                                  the checkbox or radio.
#      checked                   - (boolean) True means the button must
#                                  contain a checked="checked" attribute.
#                                  False means it must not.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertCheckboxOrRadio($$$$$$$$;$) {
    my ($xpath, $container, $pattern, $type, $name, $id, $value,
            $checked, $desc) = @_;
    my $result = '';
    my $element = getElementById($xpath, $container, $desc);
    if (!$element) {
        # Desired element doesn't exist.

        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertNodeHasPattern($xpath, $element, $pattern, $desc)) {
        $result = $::gErrorMessages;
    }
    my $nodes = $xpath->findnodes('.//input[@type="' . $type . '"]'
            . '[@name="' . $name . '"]' . '[@id="' . $id . '"]', $element);
    if ($nodes->size == 0) {
        $::gErrorMessages .= errorMessageHeader(
                "assertCheckboxOrRadio failed", $desc)
                . "----- Couldn't find $type with name=\"$name\"\n";
        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertAttribute($nodes->get_node(0), "value", $value, $desc)) {
        $result = $::gErrorMessages;
    }
    my $actualChecked = $nodes->get_node(0)->getAttribute("checked");
    if ($checked && !$actualChecked) {
        $::gErrorMessages .= errorMessageHeader(
                "assertCheckboxOrRadio failed", $desc)
                . "----- Expected $type to be checked, but it wasn't\n";
        $result = $::gErrorMessages;
    } elsif (!$checked && $actualChecked) {
        $::gErrorMessages .= errorMessageHeader(
                "assertCheckboxOrRadio failed", $desc)
                . ::outdent("
               |----- Expected no \"checked\" attribute for $type, "
                        . "found this:
                checked=\"$actualChecked\"\n");
        $result = $::gErrorMessages;
    }
    return $result;
}

# ------------------------------------------------------------------------
# assertLabeledCheckbox
#
#      This subroutine verifies the existence in an HTML document of
#      HTML generated by Html::labeledCheckbox with particular values.
#
# Results:
#      If there exists a checkbutton or radiobutton that matches $id,
#      $label, $name, and $checked, then an empty string is returned.
#      Otherwise (i.e., there was an error) one or more error messages
#      are appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      html                      - (string) Document to search.
#      label                     - (string) Expected label text (quoted
#                                  for HTML).
#      id                        - (string) Expected id for the checkbox.
#      name                      - (string) Expected name for the checkbox.
#      checked                   - (boolean) True means the checkbox should
#                                  initially be checked; false means it
#                                  shouldn't.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertLabeledCheckbox($$$$$;$) {
    my ($html, $label, $id, $name, $checked, $desc) = @_;
    my $result = '';
    my $start = "Start of markup from Html::labeledCheckbox";
    if ($html !~ m/$start[^\n]*\n([^!]*name="$name".*?)\n<!-- End/s) {
        $::gErrorMessages .= errorMessageHeader(
                "assertLabeledCheckbox failed", $desc)
                . ::outdent("
               |----- Expected checkbox with this name:
                $name
                ----- Didn't find it in this HTML:
                $html\n");
        return avoidExcessiveErrors($::gErrorMessages);
    }
    my $fragment = $1;
    if ($fragment !~ m/id="$id"/) {
        $::gErrorMessages .= errorMessageHeader(
                "assertLabeledCheckbox failed", $desc)
                . ::outdent("
               |----- Expected this id:
                $id
                ----- Didn't find it in this HTML:
                $fragment\n");
        $result = $::gErrorMessages;
    }
    if ($fragment !~ m/<span[^>]*>\s*$label/s) {
        $::gErrorMessages .= errorMessageHeader(
                "assertLabeledCheckbox failed", $desc)
                . ::outdent("
               |----- Expected this label:
                $label
                ----- Didn't find it in this HTML:
                $fragment\n");
        $result = $::gErrorMessages;
    }
    my $actualChecked = ($fragment =~ m/<input[^>]*checked="checked"/);
    if ($checked && !$actualChecked) {
        $::gErrorMessages .= errorMessageHeader(
                "assertLabeledCheckbox failed", $desc)
                . ::outdent("
               |----- Expected \"checked\" attribute in this HTML:
                $fragment\n");
        $result = $::gErrorMessages;
    } elsif (!$checked && $actualChecked) {
        $::gErrorMessages .= errorMessageHeader(
                "assertLabeledCheckbox failed", $desc)
                . ::outdent("
               |----- Expected no \"checked\" attribute in this HTML:
                $fragment\n");
        $result = $::gErrorMessages;
    }
    return $result;
}

# ------------------------------------------------------------------------
# assertTextArea
#
#      This subroutine verifies the existence in an HTML document of
#      a textarea with a given label and (possibly) an initial value.
#
# Results:
#      If there exists an element matching $id , $pattern, $name,
#      and $contents, then an empty string is returned.  Otherwise (i.e.,
#      there was an error) one or more error messages are appended to
#      the global variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      id                        - (string) id of an element containing
#                                  the input and its label.
#      pattern                   - (string) Regular expression that should
#                                  match the text contained within the
#                                  element given by $id.
#      name                      - (string) Expected "name" attribute
#                                  for a textarea element within $id.
#      contents                  - (string) Expected contents of the
#                                  textarea.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertTextArea($$$$$;$) {
    my ($xpath, $id, $pattern, $name, $contents, $desc) = @_;
    my $element = getElementById($xpath, $id, $desc);
    if (!$element) {
        # Desired element doesn't exist.

        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertNodeHasPattern($xpath, $element, $pattern, $desc)) {
        return avoidExcessiveErrors($::gErrorMessages);
    }
    my $nodes = $xpath->findnodes('.//textarea[@name="' . $name . '"]');
    if ($nodes->size == 0) {
        $::gErrorMessages .= errorMessageHeader("assertTextArea failed", $desc)
                . "----- Couldn't find textarea with name=\"$name\"\n";
        return avoidExcessiveErrors($::gErrorMessages);
    }
    my $actualContents = $nodes->get_node(0)->string_value;
    if ($actualContents ne $contents) {
        $::gErrorMessages .= errorMessageHeader(
                "assertTextArea failed", $desc)
                . ::outdent("
               |----- Expected this initial value:
                $contents
                ----- Actual initial value was this:
                $actualContents\n");
        return avoidExcessiveErrors($::gErrorMessages);
    }
    return '';
}

# ------------------------------------------------------------------------
# assertHidden
#
#      This subroutine verifies the existence in an HTML document of
#      a hidden input field with a given value.
#
# Results:
#      If there exists an element matching $id , $pattern, $name,
#      and $contents, then an empty string is returned.  Otherwise (i.e.,
#      there was an error) one or more error messages are appended to
#      the global variable $::gErrorMessages and its new value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      name                      - (string) Expected "name" attribute
#                                  for the hidden field.
#      value                     - (string) Expected value.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertHidden($$$;$) {
    my ($xpath, $name, $value, $desc) = @_;
    my $nodes = $xpath->findnodes('//form//input[@type="hidden"]'
            . '[@name="' . $name . '"]');
    if ($nodes->size == 0) {
        $::gErrorMessages .= errorMessageHeader("assertHidden failed", $desc)
                . "----- Couldn't find hidden input with name=\"$name\"\n";
        return avoidExcessiveErrors($::gErrorMessages);
    }
    if (assertAttribute($nodes->get_node(0), "value", $value, $desc)) {
        return avoidExcessiveErrors($::gErrorMessages);
    }
    return '';
}

# ------------------------------------------------------------------------
# assertTableDiagnostic
#
#      This subroutine verifies the existence in an HTML document of
#      a diagnostic message corresponding to a particular row of a
#      table.
#
# Results:
#      An empty string is returned to indicate success if the table row
#      immediately following the one given by $id contains text that
#      matches $pattern.  Otherwise (i.e., there was an error) one or more
#      error messages are appended to the global variable $::gErrorMessages
#      and its new value is returned.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      id                        - (string) id of a "tr" element for which
#                                  there is supposed to be a diagnostic
#                                  message.
#      pattern                  - (string) Regular expression identifying
#                                  the expected diagnostic message.
#      desc                      - (string) Optional string to include
#                                  in error messages.
# ------------------------------------------------------------------------

sub assertTableDiagnostic($$$;$) {
    my ($xpath, $id, $pattern, $desc) = @_;
    my $text = $xpath->findvalue(
            '//tr[@id="' . $id . '"]/following-sibling::*[1]');
    if ($text =~ $pattern) {
        return '';
    }
    $::gErrorMessages .= errorMessageHeader("assertTableDiagnostic failed",
            $desc) . ::outdent("
           |----- Pattern for error diagnostic:
            $pattern
            ----- Didn't match text of following row:
            $text\n");
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertOK
#
#       Asserts that an XML XPath has at least one /responses/response entry
#       and no /responses/error entries.
#
# Arguments
#       response - (XML::XPath) xpath object from api request, either the
#       toplevel responses element, or an individual response or error
#       element.
#       desc            - (string) description for assertion failure
# ------------------------------------------------------------------------

sub assertOK($;$) {
    my ($response, $desc) = @_;

    if (!defined($response)) {
        $::gErrorMessages .= errorMessageHeader("assertOK failed", $desc)
                . "----- No response";
        return avoidExcessiveErrors($::gErrorMessages);
    } else {
        # If the top-level element isn't 'responses' then assume single
        # response/error element (from parsing a batch response)
        if ($response->findnodes('responses')->size() == 0) {
            if ($response->getName() eq 'error') {
                $::gErrorMessages .= errorMessageHeader("assertOK failed", $desc)
                    . "----- <error> elements in message:\n"
                    . $response->toString() . "\n";
                return avoidExcessiveErrors($::gErrorMessages);
            } elsif ($response->getName() ne 'response') {
                $::gErrorMessages .= errorMessageHeader("assertOK failed", $desc)
                    . "----- No <response> elements in message:\n"
                    . $response->toString() . "\n";
                return avoidExcessiveErrors($::gErrorMessages);
            }
        }
        else {
            if ($response->findnodes('responses/error')->size() > 0) {
                $::gErrorMessages .= errorMessageHeader("assertOK failed", $desc)
                    . "----- <error> elements in message:\n"
                    . $response->findnodes_as_string("/") . "\n";
                return avoidExcessiveErrors($::gErrorMessages);
            } elsif ($response->findnodes('responses/response')->size() == 0) {
                $::gErrorMessages .= errorMessageHeader("assertOK failed", $desc)
                    . "----- No <response> elements in message:\n"
                    . $response->findnodes_as_string("/") . "\n";
                return avoidExcessiveErrors($::gErrorMessages);
            }
        }
    }
}

# ------------------------------------------------------------------------
# assertJavascriptInclude
#
#      This subroutine verifies that a particular Javascript file is
#      incorporated in a given Web page.
#
# Results:
#      An empty string is returned if there is HTML in $html to
#      incorporate $fileName.  Otherwise an error message is appended
#      to the global variable $::gErrorMessages.
#
# Arguments:
#      fileName                  - (string) Regexp pattern for the name
#                                  of the desired Javascript file.
#      html                      - (string) Document that should include
#                                  the Javascript file.
#      desc                      - (string) Display this in the error
#                                  message if the file isn't included.
# ------------------------------------------------------------------------

sub assertJavascriptInclude($$;$) {
    my ($fileName, $html, $desc) = @_;
    if ($html =~ m/<script[^>]*src="[^"]*$fileName[^"]*"/) {
        return;
    }
    $::gErrorMessages .= errorMessageHeader("assertJavascriptInclude failed",
            $desc) . ::outdent("
           |----- Javascript file:
            $fileName
            ----- Is not included by the document:
            $html\n");
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertJavascriptVariable
#
#      This subroutine verifies that a particular Javascript variable
#      is defined in a page with a particular value.
#
# Results:
#      An empty string is returned if $pattern matches the text of $node.
#      Otherwise an error message is appended to the global variable
#      $::gErrorMessages.
#
# Arguments:
#      variableName              - (string) Name of a global Javascript
#                                  variable.
#      value                     - (string) Expected initial value for
#                                  the variable.
#      html                      - (string) Document to search for the
#                                  variable definition.
#      desc                      - (string) Display this in any error
#                                  message that is generated.
# ------------------------------------------------------------------------

sub assertJavascriptVariable($$;$) {
    my ($variableName, $value, $html, $desc) = @_;

    # Extract the start of code, then check in it for the variable
    # definition

    my $pattern = 'Start of code from Js::startupCode[^>]*>\n(.*)'
            . '<!-- End of code from Js::startupCode';
    if ($html !~ m/$pattern/s) {
        $::gErrorMessages .= errorMessageHeader(
                "assertJavascriptVariable failed", $desc) . ::outdent("
            |----- No Javascript startup code in document:
                $html\n");
        return avoidExcessiveErrors($::gErrorMessages);
    }
    my $string = $variableName . " = " . $value . ";\n";
    my $string2 = "[\"$variableName\"]" . " = " . $value . ";\n";
    if (index($html, $string) >= 0 || index($html, $string2) >= 0) {
        return '';
    }
    $::gErrorMessages .= errorMessageHeader("assertJavascriptVariable failed",
            $desc) . ::outdent("
           |----- Expected this variable:
            $variableName
            ----- to be defined with this value:
            $value
            ----- in this Javascript startup code:
            $1\n");
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertCssInclude
#
#      This subroutine verifies that a particular CSS file is incorporated
#      in a given Web page.
#
# Results:
#      An empty string is returned if there is HTML in $html to
#      incorporate $fileName.  Otherwise an error message is appended
#      to the global variable $::gErrorMessages and the new value of
#      $::gErrorMessages is returned.
#
# Arguments:
#      fileName                  - (string) Regexp pattern for the name
#                                  of the desired CSS file.
#      html                      - (string) Document that should include
#                                  the CSS file.
#      desc                      - (string) Display this in the error
#                                  message if the file isn't included.
# ------------------------------------------------------------------------

sub assertCssInclude($$;$) {
    my ($fileName, $html, $desc) = @_;
    if ($html =~ m/<link href="[^"]*$fileName[^"]*" rel="stylesheet"/) {
        return;
    }
    $html =~ m|(<head>.*</head>)|s;
    $::gErrorMessages .= errorMessageHeader("assertCssInclude failed",
            $desc) . ::outdent("
           |----- Stylesheet:
            $fileName
            ----- Is not included in the document header:
            $1\n");
    return avoidExcessiveErrors($::gErrorMessages);
}

sub assertAccessDenied($$;$) {
    my ($xpath, $desc, $errMsg) = @_;
    assertEq('AccessDenied', $xpath->findvalue('//code'), "$desc code");

    if (defined($errMsg)) {
        assert($errMsg, '=~', $xpath->findvalue('//message'), "$desc message");
    }
}

sub assertNotSettable($$) {
    my ($xpath, $desc) = @_;
    assertEq('PropertyNotSettable', $xpath->findvalue('//code'), $desc);
}

# ------------------------------------------------------------------------
# assertHeader
#
#      This subroutine is invoked by a test to verify the contents of
#      a header element.
#
# Results:
#      An empty string is returned to indicate success if the test
#      succeeds.  Otherwise (i.e., there was an error) one or more
#      error messages are appended to the global variable $::gErrorMessages
#      and its new value is returned.
#
# Required Arguments:
#      xpath           - (XML::XPath object) An object containing the
#                        parsed XML of the HTML page.
#      id              - (string) The HTML id of the header.
#      class           - (string) The CSS class of the header.
#
# Optional Arguments (passed as any number of name, value pairs):
#      expandable      - (boolean) Verify whether or not the header is
#                        expandable.
#      title           - (string) The text for the primary title.
#      title2          - (string) Text for the secondary title.
#      titleLinkText   - (string) Text for a link displayed in the title.
#      titleLinkUrl    - (string) Url for a link displayed in the title.
#      numActions      - (integer) The number of actions in the header.
# ------------------------------------------------------------------------

sub assertHeader($$$;@) {
    my ($xpath, $id, $class, @args) = @_;
    my $desc = "header '$id'";

    # Verify the CSS class:

    assertEq($class, $xpath->findvalue('//div[@id="' . $id
        . '"]/@class'), $desc . " - class");

    for (my $i = 0; $i < scalar(@args); $i += 2) {
        my $name = $args[$i];
        my $value = $args[$i+1];
        switch($name) {
            case "expandable" {
                assertEq($value, $xpath->findnodes('//div[@id="' . $id
                    . '"]//td[@class="header_left"]//td[1]//img')->size(),
                    $desc . " - expandable");
            }
            case "title" {
                assertEq($value, $xpath->findvalue(
                    '//div[@id="' . $id . '"]//td[@class="header_title"]'),
                    $desc . " - main title");
            }
            case "title2" {
                assertEq($value, $xpath->findvalue(
                    '//div[@id="' . $id . '"]//td[@class="header_title2"]'),
                    $desc . " - secondary title");
            }
            case "titleLinkText" {
                assertEq(": " . $value, $xpath->findvalue(
                    '//div[@id="' . $id . '"]'
                    . '//td[@class="header_titleWithLink"]'),
                    $desc . " - text for title link");
            }
            case "titleLinkUrl" {
                assertEq($value, $xpath->findvalue(
                    '//div[@id="' . $id . '"]'
                    . '//td[@class="header_titleWithLink"]'
                    . '//a/@href'),
                    $desc . " - text for title link");
            }
            case "numActions" {
                my @urls = getHeaderActions($xpath, $id);
                assertEq($value, scalar(@urls), $desc . " - # actions");
            }
            else {
                # An invalid option was used.  Append a nicely formatted
                # message to the test result log and return it.

                $::gErrorMessages .= errorMessageHeader("assertHeader failed",
                    "Invalid option") . "The option \"$name\" is not valid.\n"
                    . "Valid options: \"expandable\", \"title\", \"title2\", "
                    . "\"titleLinkText\", \"titleLinkUrl\", \"numActions\".";
                return avoidExcessiveErrors($::gErrorMessages);
            }
        }
    }
    return "";
}

# ------------------------------------------------------------------------
# assertHeaderAction
#
#      This subroutine is invoked by a test to test the contents of
#      a single header action.
#
# Results:
#      An empty string is returned to indicate success if the test
#      succeeds.  Otherwise (i.e., there was an error) one or more
#      error messages are appended to the global variable $::gErrorMessages
#      and its new value is returned.
#
# Arguments:
#      xpath           - (XML::XPath object) An object containing the
#                        parsed XML of the HTML page.
#      id              - (string) The HTML id of the header.
#      url             - (string) The target URL of the action.
#      desc            - (string) Display this in the error message if
#                        the test fails.
# ------------------------------------------------------------------------

sub assertHeaderAction($$$;$) {
    my ($xpath, $id, $url, $desc) = @_;
    my $urlFound = 0;

    my @urls = getHeaderActions($xpath, $id);

    for (my $i = 0; $i < scalar(@urls); $i ++) {
        if (index($urls[$i], $url) == 0) {
            return avoidExcessiveErrors($::gErrorMessages);
        }
    }

    # The URL was not found.  Append a message to the global error messages
    # and return it.

    $::gErrorMessages .=
            errorMessageHeader("assertHeaderAction failed", $desc)
            . "----- URL that should have appeared in list:\n$url\n"
            . "----- List of URLS for actions from test:\n";
    foreach my $element (@urls) {
        $::gErrorMessages .= $element . "\n";
    }
    return avoidExcessiveErrors($::gErrorMessages);
}

# ------------------------------------------------------------------------
# assertXpath
#
#      This subroutine is invoked by a test to verify that the test
#      performed correctly.  It performs an xpath findvalue, compares
#      it to the expected value, and prints the XML if the test fails.
#
#
# Results:
#      If "expected" and "actual" are "eq", then an empty string is
#      returned.  Otherwise (i.e., there was an error) an error message
#      is appended to the global variable $::gErrorMessages and its new
#      value is returned.
#
# Arguments:
#      expectedValue  - (string) Value against which to compare $testValue
#      xpath          - (XPath)  XPath object containing value to check
#      path           - (string) Path to that value
# ------------------------------------------------------------------------

sub assertXpath {
    my ($expected, $xpath, $path) = @_;
    assertEq($expected, $xpath->findvalue($path), "\"$path\" in:\n"
             . $xpath->findnodes_as_string("/"));
}

# ------------------------------------------------------------------------
# getHeaderActions
#
#      Return all of the URLs found in links in a specified set of elements.
#
# Results:
#      If any of the specified elements contain <a href=...>
#      elements, return a list of URLs from those elements.  If no
#      matching hrefs are found, return an empty list.
#
# Arguments:
#      xpath                     - (XML::XPath) Document containing the
#                                  table.
#      path                      - (string) XPath string identifying elements
#                                  in which to search for links.
#                                  element(s) for the table.  If there
#                                  are multiple such elements, each is
#                                  searched and first matching element
#                                  is returned.
# ------------------------------------------------------------------------

sub getHeaderActions($$) {
    my ($xpath, $id) = @_;

    # Look for actions in three different paths, since headers may use
    # different styles which store actions in either a div or a table:

    my $path1 = '//div[@id="' . $id . '"]//div[@class="actionList"]';
    my $path2 = '//div[@id="' . $id . '"]//table[@class="actionList"]'
            . '//td[@class="action"]';
    my $path3 = '//div[@id="' . $id . '"]//table[@class="actionListInline"]'
            . '//td[@class="action"]';
    my @actions = ($xpath->findnodes($path1)->get_nodelist,
            $xpath->findnodes($path2)->get_nodelist,
            $xpath->findnodes($path3)->get_nodelist);

    my @urls = ();

    foreach my $context (@actions) {
        foreach my $href ($xpath->findnodes(".//a", $context)->get_nodelist) {
            my $url = $href->getAttribute("href");
            if ($url eq "") {
                # An "onclick" handler may be used instead of an "href".

                $url = $href->getAttribute("onclick");
                if ($url eq "") {
                    next;
                } else {
                    my $index = index($url, "window.location.href = ");
                    if ($index >= 0) {
                        $url = substr($url, $index + 24, -2);
                    }
                }
            }
            push(@urls, $url);
        }
    }
    return @urls;
}

# ------------------------------------------------------------------------
# avoidExcessiveErrors
#
#       die if the test is producing a heinous amount of output.
#
# Results:
#       If the error string is getting too large, assume we have a runaway
#       test and call die() to terminate the test.
#
# Arguments:
#       errorMessages - (string) The error message string.
#
# Returns:
#       If die() isn't invoked, returns the string unchanged.
# ------------------------------------------------------------------------

sub avoidExcessiveErrors($) {

    my ($errorMessages) = @_;

    if (defined($errorMessages) && length($errorMessages) > 40960000) {
        die("Too many errors\n");
    }
    else {
        return $errorMessages;
    }
}

1;
