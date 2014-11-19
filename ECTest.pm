# -*-Perl-*-

# ECTest.pm
#
# Helper routines for running tests against a commander server.
#
# Copyright (c) 2010 Electric Cloud, Inc.
# All rights reserved
#

package ECTest;

use strict;
use warnings;
use Exporter;
use Assert;
use ElectricCommander;
use ElectricCommander::Util;

our @ISA = ('Exporter');

our @EXPORT = qw(
                    $adminSession
                    $guestSession
                    $guestUser
                    $guestPassword
                    checkServerVersion
                    createDefaultWorkspace
                    createTestResource
                    getStepWorkspace
                    initCommander
                    loginUser
                    newSession
                    readStepLogFile
                    runAndWaitForJob
                    waitForJob
                    waitForJobStatus
            );

# The following sessions are available for test use and are initialized
# by initCommander
our $adminSession;
our $guestSession;

# The non-admin user account/password
our $guestUser = 'guest';
our $guestPassword = 'CommanderRocks';

my $initCommanderCalled;
my $serverVersion;

# Default timeout used in wait* routines
my $gTimeout = 600;


=head1 NAME

ECTest - utility functions for testing against a commander server

=head1 SYNOPSIS

 use ECTest;

 # Set up admin and guest logins
 initCommander;
 my $guest = $guestSession;
 my $admin = $adminSession;

 # Create a new guest session
 my $session = newSession($guestUser, $guestPassword);

=head1 DESCRIPTION

=head2 initCommander

Initialize default sessions.  Ensure that the guest user exists and
has full access to the Default project.  Ensure that all users have
read access to the server.

=cut

sub initCommander()
{
    if ($initCommanderCalled) {
        return;
    }

    $::gLogTestNames = 1;
    $::gLogServerStats = 0;

    $adminSession = newSession('admin', 'changeme');

    # Import a license file

    #$adminSession->deleteLicense('ElectricCommander', 'Server');
    #my $licenseData = `cat $::gTestFileDir/../../build/license.xml`;
    #my $licenseData = `cat $::gNTestDir/../license.xml`;
    #$adminSession->importLicenseData("$licenseData");

    # Grant everyone execute permission
#    $adminSession->createAclEntry('group', 'Everyone',
#                                  {systemObjectName => 'server',
#                                   executePrivilege => 'allow'});

    # Create the guest user with full rights

    $adminSession->deleteUser($guestUser);
    $adminSession->createUser($guestUser, {password => $guestPassword});
    $adminSession->deleteAclEntry('user', $guestUser,
                                  { systemObjectName => 'server' });

    # Grant the guest user full rights on the Default project

    $adminSession->createProject('Default');
    $adminSession->deleteAclEntry('user', $guestUser, {
        projectName => 'Default' });
    $adminSession->createAclEntry('user', $guestUser, {
        projectName                => 'Default',
        readPrivilege              => 'allow',
        modifyPrivilege            => 'allow',
        executePrivilege           => 'allow',
        changePermissionsPrivilege => 'allow'});

    $guestSession = newSession($guestUser, $guestPassword);

    $initCommanderCalled = 1;
};

=head2 newSession USER PASSWORD

Create a new connection to the server and login as the specified user.

=cut

sub newSession($$)
{
    my ($user, $password) = @_;
    my $session = new ElectricCommander({
        server => $::gTarget,
        debug => $::gDebug,
        logFile => $::gDebugFile,
    });
    $session->login($user, {password => $password});
    
    return $session;
}

=head2 getStepWorkspace XPATH STEPNAME

Get the step workspace.

=cut

# ------------------------------------------------------------------------
# getStepWorkspace
#
#       Get the step workspace.
#
# Arguments
#       xpath         - DOM with a getJobDetails response for the relevant
#                       job.
#       stepName      - Name or id of the step
# ------------------------------------------------------------------------

sub getStepWorkspace {
    my ($xpath, $stepName) = @_;

    my $stepPath = "//jobStep[stepName='$stepName' or jobStepId='$stepName']";
    my $workspaceName = $xpath->findvalue("$stepPath/workspaceName");
    my $pathVar = ($^O eq "MSWin32") ? 'winDrive' : 'unix';
    return $xpath->findvalue("//job/workspace[workspaceName='$workspaceName']"
                          . "/$pathVar");
}

=head2 readStepLogFile XPATH STEPNAME

Read the step log file for the given step name.

=cut

# ------------------------------------------------------------------------
# readStepLogFile
#
#       Read the step log file for the given step name.
#
# Arguments
#       xpath         - DOM with a getJobDetails response for the relevant
#                       job.
#       stepName      - Name or id of the step whose log we want to read.
# ------------------------------------------------------------------------

sub readStepLogFile {
    my ($xpath, $stepName) = @_;

    my $stepPath = "//jobStep[stepName='$stepName' or jobStepId='$stepName']";
    my $workspaceRoot = getStepWorkspace($xpath, $stepName);
    my $stepLogFile = "$workspaceRoot/"
        . $xpath->findvalue("$stepPath/logFileName");

    mesg('DBG', "Reading step log file: $stepLogFile\n");

    return ::readFile("$stepLogFile");
}

=head2 checkServerVersion

Compare the minimum server version specified in the plugin.xml file
for the current module to the version reported by the server.  If the
server isn't compatible, skip all tests.

=cut

# -------------------------------------------------------------------------
# checkServerVersion
#
#       Compare the minimum server version specified in the plugin.xml
#       file for the current module to the version reported by the server.
#       If the server isn't compatible, skip all tests.
# -------------------------------------------------------------------------
sub checkServerVersion()
{
    # If we've already checked the server version, there's nothing to do
    if ($serverVersion) {
        return;
    }

    my $file = $::gTestFileDir."/../META-INF/plugin.xml";
    if (! -f $file) {
        $file = $::gTestFileDir."/../src/main/resources/META-INF/plugin.xml";
    }
    my $xpath = XML::XPath->new(filename => $file);
    my $required = $xpath->findvalue('//commander-version/@min')->value;

    if ($required ne '') {
        $xpath = $adminSession->getVersions();
        my $serverVersion = $xpath->findvalue('//version')->value;
        if (compareExact($serverVersion, $required) < 0) {
            print "Commander version ($serverVersion) is not compatible with the minimum version\nrequired by the plugin ($required), skipping all tests\n\n";
            $::gStart = "SKIP_ALL";
        }
    }
}

=head2 loginUser SESSION USERNAME PASSWORD TIMEOUT

Login to the ElectricCommander server.

=cut

# ------------------------------------------------------------------------
# loginUser
#       Login to the ElectricCommander server.
#
# Arguments:
#       session  - (ElectricCommander object) Session to login with
#       userName - (string) The user name.
#       password - (string) The password.
#       timeout  - (int) Optional timeout.  Defaults to current session
#                        timeout.
# ------------------------------------------------------------------------
             
sub loginUser($$$;$) {
    my ($session, $userName, $password, $timeout) = @_;
             
    # Timeout is optional
    if (defined($timeout)) {
        $timeout = $session->setTimeout($timeout);
    }
             
    my $xpath = $session->login($userName, $password);
             
    if (!defined($xpath)) {
        my $msg = "ERROR: Failed to log into ElectricCommander." .
            "  No response from the server: " . $session->getError() . "\n\n";
        $msg .= isWindows() ? `ps -W` : `ps -ef`;
        $msg .= "\n\n";
        $msg .= `netstat -an | grep LISTEN`;
        print $msg;
    }
    else {
        if ($xpath->findnodes('/responses/response/sessionId')->size() == 0) {
            # Some kind of error occurred.
             
            print('ERROR: Failed to log into ElectricCommander: ' .
                  $xpath->findvalue('//code') . "\n");
        }
    }
             
    # Restore the timeout if we set it.
    if (defined($timeout)) {
        $session->setTimeout($timeout);
    }
}


=head2 createDefaultWorkspace SESSION

Creates the "default" workspace.

=cut

#-----------------------------------------------------------------------------
# createDefaultWorkspace -- ensures that "default" workspace exists
#-----------------------------------------------------------------------------

sub createDefaultWorkspace {
    my ($session) = @_;

    $session->deleteWorkspace('default');
    $session->createWorkspace('default', {
        agentDrivePath => $ENV{COMMANDER_WORKSPACE_WINDRIVE},
        agentUncPath => $ENV{COMMANDER_WORKSPACE_WINUNC},
        agentUnixPath => $ENV{COMMANDER_WORKSPACE_UNIX}
    });
}


=head2 createTestResource SESSION AGENTNAME AGENTHOST

Creates a resource for an agent host.

=cut

# ------------------------------------------------------------------------
# createTestResource -- ensures that "$gAgentHost" resource exists
# ------------------------------------------------------------------------

sub createTestResource {
    my ($session, $agentName, $agentHost) = @_;

    $session->deleteResource($agentName);

    return $session->createResource($agentName,
				    {hostName  => $agentHost,
				     stepLimit => 1,
				     block     => 1});
}


=head2 runAndWaitForJob SESSION PROJECT RUNNABLE TIMEOUT ISSCHEDULE

Runs a procedure or schedule and waits until it completes.

Asserts if the job doesn't end up in 'completed' status or the timeout
elapses, and then aborts the job (so future tests aren't screwed up by
a stalled job)

=cut

# ------------------------------------------------------------------------
# runAndWaitForJob
#
#       Runs a procedure or schedule and waits until it completes.
#
#       Asserts if the job doesn't end up in 'completed' status or the timeout
#       elapses, and then aborts the job (so future tests aren't screwed up by
#       a stalled job)
#
# Arguments
#     session
#     project           - (string) project containing procedure to run.
#     procedure         - (string) procedure to run.
#     timeout           - (int) timeout in seconds, defaults to gTimeout.
#     isSchedule        - (int) whether or not the 'procedure' arg is actually
#                         a schedule name.  Defaults to 0.
# ------------------------------------------------------------------------

sub runAndWaitForJob($$;$$) {
    my ($session, $project, $runnable, $timeout, $isSchedule) = @_;

    $timeout    = $gTimeout unless defined($timeout);
    $isSchedule = 0         unless defined($isSchedule);

    my $toRun = $isSchedule ? 'scheduleName' : 'procedureName';
    my $xpath = $session->runProcedure($project, {
        $toRun       => $runnable,
        pollInterval => '0.2',
        timeout      => $timeout});

    assertDef($xpath, "runProcedure finished without a timeout");

    if ($xpath) {
        my $jobId  = $xpath->findvalue('//jobId');
        my $status = $xpath->findvalue('//status');

        assertTrue($jobId && int($jobId) > 0, 'valid job id');
        assertEq('completed', $status, "job $jobId completed");

        # Abort the job if it hasn't completed
        if ($status ne 'completed') {
            assertOK($session->abortJob($jobId, {force => 1}));
        }
    }

    return $xpath;
}


# ------------------------------------------------------------------------
# waitForJobStatus
#
#       Blocks until a job is in a particular status, with a timeout.
#
# Arguments
#       jobId           - (int)    the id of the job
#       status          - (string) the status to wait for
#       timeout         - (int)    timeout in seconds, defaults to gTimeout.
# ------------------------------------------------------------------------

sub waitForJobStatus($$;$) {
    my ($jobId, $status, $timeout) = @_;

    $timeout = $gTimeout unless defined($timeout);

    mesg('DBG', "waitForJobStatus $jobId $status $timeout\n");

    my $xpath;
    my $current;

    # Make sure jobId is valid; otherwise no point bugging the server
    # with illegal requests.
    assert("", "ne", $jobId, "waitForJobStatus: jobId is non-empty");
    assertDef($jobId, "waitForJobStatus: jobId is defined");
    if (!$jobId) {
        return $xpath;
    }

    if ($status eq 'completed' or $status eq 'running') {
        $xpath = $adminSession->waitForJob($jobId, $timeout, $status);

        if (!defined $xpath) {
            $::gErrorMessages .= $adminSession->getError . "\n";
            return;
        }

        mesg('DBG', "Finished waiting on job $jobId to reach $status\n");

        $current = $xpath->findvalue('/responses/response/status');

        if ($status eq 'completed' && $current ne 'completed') {
            # The job should've completed, but it didn't.  Force-abort
            # it to keep it from adversely affecting subsequent tests.

            $adminSession->abortJob($jobId, {force => 1});
        }
    } else {

        # Loop until the job reaches the status, or we time out
        my $start = time();

        while (time() < $start + $timeout) {

            $xpath = $adminSession->getJobStatus($jobId);

            assertOK($xpath);

            $current = $xpath->findvalue('/responses/response/status');

            if ($current =~ /$status/) {
                mesg('DBG', "Finished waiting on job $jobId to reach $status\n");
                last;
            } elsif ($current ne '') {
                mesg('DBG', "Waiting on job $jobId to reach $status\n");
                sleep 0.5;
                next;
            } else {
                $::gErrorMessages .= "No such job: '$jobId'\n";
                return;
            }
        }
    }
    assert($status, "=~", $current,
             "Timeout waiting on job $jobId to reach $status");

    return $xpath;
}

# ------------------------------------------------------------------------
# waitForJob
#
#       Blocks until a job runs to completion (status = completed), with
#       timeout.
#
# Arguments
#       jobId           - (int) the id of the job.
#       timeout           - (int) timeout in seconds, defaults to gTimeout.
# ------------------------------------------------------------------------

sub waitForJob($;$) {
    my ($jobId, $timeout) = @_;

    return waitForJobStatus($jobId, "completed", $timeout);
}

1;
