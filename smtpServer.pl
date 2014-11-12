# -*-Perl-*-

# smtpServer.pl
#
# Auxilliary helper for 'ntest'
# Used to verify the delivery of Notiers emails.
#
# Usage:
#    smtpServer --log logFile --upfile upFile --port port --recordfile=
#
# Copyright (c) 2005-2006 Electric Cloud, Inc.
# All rights reserved

#use strict;
use warnings;
use File::Copy;
use Getopt::Long;
use HTTP::Daemon;
use Data::Dumper;
use XML::Simple qw(:strict);
use XML::XPath;
use Cwd;
use FindBin;
use Carp;
use FileHandle;
use Net::SMTP;                          #::Server;

my $recordSep       = '\\|\\|';
my $fieldSep        = '\\|';
my $jsRecordSep       = '\\:\\:\\:\\:';
my $jsFieldSep        = '\\:\\:';
my $testNameSep     = '===';

use ElectricCommander;

# The following nested package implements a subset of the ESTMP protocol
# It handles the basic handshake, declares support for AUTH LOGIN
# authentication, and verifies against an expected credential of
#      'gooduser', 'goodpass'
{
    package SMTPClient;
    use MIME::Base64;

    my %smtpCmds = (
                    MAIL => \&_mail,
                    RCPT => \&_rcpt,
                    DATA => \&_data,
                    QUIT => \&_quit,
                    EHLO => \&_ehlo,
                    AUTH => \&_auth,
                    );

    # Send a response
    sub _put {
        print {shift->{SOCK}} @_, "\r\n";
    }

    # Parse a from/to line
    sub _fromto {
        my $self = shift;
        my($which, $var, $args) = @_;

        if (!($$args[0] =~ /^$which\s*([^\s]+)/i)) {
            if (!$$args[1] || !($$args[0] =~ /^$which$/i)) {
                $self->_put("501 Malformed $which line");
                return -1;
            }

            ref($var) eq 'ARRAY' ? (push @$var, $$args[1]) : ($$var = $$args[1]);
        }

        ref($var) eq 'ARRAY' ? (push @$var, $1) : ($$var = $1) unless !defined($1);

        if ($which eq "TO:") {
            # If all of the destination addresses end in '@anon', then allow
            # anonymous authentication.

            my $anon = 1;
            foreach my $dest (@$var) {
                if ($dest !~ m/\<.*\@anon\>/) {
                    $anon = 0;
                }
            }

            if (!$anon && !$self->{AUTH}) {
                $self->_put("550 Unable to relay");
                return;
            }
        }
        $self->_put("250 Completed.");
    }

    # Respond to the EHLO capabilities query
    sub _ehlo {
        ::mesg("EHLO $@\n");
        my $self = shift;
        $self->_put("250-localhost Hello");
        $self->_put("250-AUTH LOGIN");
        $self->_put("250-AUTH=LOGIN");
        $self->_put("250 OK");
    }

    # Read the next line, ignoring empty lines
    sub nextCommand {
        my $self = shift;
        my $sock = $self->{SOCK};
        while (<$sock>) {
            s/\r\n//;
            if ($_ ne "") {
                return $_;
            }
        }
    }

    # Process presented credentials
    sub _auth {
        ::mesg("AUTH\n");
        my $self = shift;
        my $sock = $self->{SOCK};
        $self->_put("334 " . encode_base64('Username:',''));
        my $user = decode_base64($self->nextCommand);
        $self->_put("334 " . encode_base64('Password:',''));
        my $pw = decode_base64($self->nextCommand);
        if ($user eq "gooduser" && $pw eq "goodpass") {
            $self->_put("235 Authentication successful.");
            $self->{AUTH} = $user;
        } else {
            $self->_put("535 Authentication unsuccessful.");
            $self->{AUTH} = undef;
        }
    }

    sub _mail {
        ::mesg("MAIL\n");
        my $self = shift;
        return $self->_fromto('FROM:', \$self->{FROM}, @_);
    }

    sub _rcpt {
        ::mesg("RCPT\n");
        my $self = shift;
        return $self->_fromto('TO:', \@{ $self->{TO} }, @_);
    }

    sub _data {
        ::mesg("DATA\n");
        my $self = shift;
        my $done = undef;

        if (!defined($self->{FROM})) {
            $self->_put("503 Missing FROM");
            return 1;
        }

        if (!defined(@{$self->{TO}})) {
            $self->_put("503 Missing RCPT");
            return 1;
        }

        $self->_put("354 Start mail input, end with <CRLF>.<CRLF>");

        my $sock = $self->{SOCK};

        while (<$sock>) {
            if (/^\.\r\n$/) {
                $done = 1;
                last;
            }

            # RFC 821 compliance.
            s/^\.\./\./;
            $self->{MSG} .= $_;
        }

        if (!defined($done)) {
            $self->_put("550 Requested action not taken");
            return 1;
        }

        $self->_put("250 Completed");
    }

    sub _quit {
        ::mesg("QUIT\n");
        my $self = shift;

        $self->_put("221 Service closing transmission channel.");
        $self->{SOCK}->close;
        return 0;
    }

    sub new {
        my($this, $sock) = @_;

        my $class = ref($this) || $this;
        my $self = {};
        $self->{FROM} = undef;
        $self->{TO} = [];
        $self->{MSG} = undef;
        $self->{SOCK} = $sock;
        $self->{AUTH} = undef;

        bless($self, $class);

        croak("No client connection specified.") unless defined($self->{SOCK});
        $self->_put("220 Test SMTP Ready.");
        return $self;
    }

    sub process {
        my $self = shift;
        my($cmd, @args);

        my $sock = $self->{SOCK};

        while (<$sock>) {
            # Clean up.
            chomp;
            s/^\s+//;
            s/\s+$//;
            goto bad unless length($_);

            ($cmd, @args) = split(/\s+/);

            $cmd =~ tr/a-z/A-Z/;

            if (!defined($smtpCmds{$cmd})) {
              bad:
                $self->_put("500 Unsupported command: $cmd");
                next;
            }

            return(defined($self->{MSG}) ? 1 : 0) unless
                &{$smtpCmds{$cmd}}($self, \@args);
        }
        return undef;
    }
}

# ------------------------------------------------------------------------
# Globals
# ------------------------------------------------------------------------

my $gLog  = "";           # by default, send output to stderr
my $gPort = "";           # default port to listen on
my $gUpfile = "";         # by default, don't create upfile once running
my $gRecfile = "";        # record of all emails received
my $gTestname = "";       # the modtest name

# ------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------

my %gOptions =
    (
     # --log=<path>
     # If specified, send stderr to the specified file.
     "log=s"                                 => \$gLog,

     # --upfile=<path>
     # Once the server is started, touch the specified file.
     "upfile=s"                              => \$gUpfile,

     # --recordfile=<path>
     # File where smtpServer save records of emails received.
     "recordfile=s"                          => \$gRecfile,


     # --test=<name>
     # The modtest name for this SMTP server execution.
     "test=s"                          => \$gTestname,
     );

# ------------------------------------------------------------------------
# mesg
#
#      Write out message to log file.
# ------------------------------------------------------------------------

sub mesg($) {
    my ($mesg) = @_;
    print (STDERR ("\n[smtpServer] " . localtime() . " " . $mesg));
}

# ------------------------------------------------------------------------
# main
# ------------------------------------------------------------------------

sub main {

    # Save original arguments so we can write to log after processing.

    my @originalArgs = @ARGV;
    my $recordFileName = '';

    # Parse command line arguments into global variables.

    GetOptions(%gOptions);

    # If a logfile was specified, send STDERR there.

    if ($gLog ne "") {
        open (STDERR, ">$gLog");
    }

    mesg ("starting");
    mesg ("cwd: " . cwd() . "");
    mesg ("args: " . join(' ', @originalArgs) . "");

    # Create new server.

    my $port = $ARGV[0] || 0;
    mesg("trying for port $port\n");

    mesg ("smtpServer ...Starting... for test: " . $gTestname . "  and log file=" . $gLog . "\n");

    my $server = Net::SMTP::Server -> new ('localhost', $port) || croak("Unable to handle client connection: $!\n");

    mesg ("smtpServer  ...Created Server...\n");

    $gPort = $server->{SOCK}->sockport();

    # print the listening port on stdout so the parent process can pick it up.
    $| = 1;

    print "listening on port $gPort\n";

    mesg ("the SMTP server is using port $gPort");

    my $conn = '';

    mesg ("smtpServer  ...in loop  ...\n");

    while( $conn = $server->accept()) {

        mesg ("smtpServer  ...Accepted Client Connection...\n");

        # Handle the client's connection and spawn off a new parser.
        # This can/should be a fork() or a new thread,
        # but for simplicity...
        my $client = new SMTPClient($conn) ||
            croak("Unable to handle client connection: $!\n");

        mesg ("smtpServer  ...Handling Client Connection...\n");

        # Process the client.  This command will block until
        # the connecting client completes the SMTP transaction.
        $client->process || next;

        mesg ("smtpServer  ...Processed Client Message...\n");

        # Trace the message received
        mesg ("smtpServer  ...----------------Client Message ---------------------\n" .
              "FROM:       " .  $client->{FROM}     . "\n" .
              "TO:         " .  join(", ", @{$client->{TO}}) . "\n" .
              "MSG:        " .  $client->{MSG}      . "\n" .
              "USER:       " .  $client->{USER}. "\n" .
              "PASSWORD:   " .  $client->{PASSWORD} . "\n");

        my $from      = $client->{FROM} ;
        my $message   = $client->{MSG};

        # Check if the mail need special interpretation
        my $command = "mail" ;
        if ( $from eq '<controller-reset@electric-cloud.com>' ) {
            $command = "reset";
        }
        elsif ( $from eq '<controller-stop@electric-cloud.com>' ) {
            $command = "stop";
        }

        mesg ("smtpServer ... Found command:  $command \n");

        if ( $command eq "mail" ) {

            my @records = split $recordSep, $message;

            my $record ;
            if ( $#records == 2 ) {
                $record = $records[1];
                mesg ("smtpServer ... Found mail record: \n<<" . $record . ">>\n");
                #print( RECORDFILE "$record" . '||' . "\n");
            }
            else {
                mesg ("--- ERROR: Bad mail: number of records: $#records not as expected: 2\n");
            }
            # Collect the Javascript properties part of the email record

            my @jsSegments = split $jsRecordSep, $message;

            mesg ("smtpServer ... No of lines with Javascript properties + 1 is: $#jsSegments\n") ;

            if ( $#jsSegments == 7 ) {
                my @jsArray = ();
                my $seg;
                my $i;
                for( $i=0; $i < @jsSegments -1 ; $i++) {

                    $seg = $jsSegments[ $i];
                    mesg ("smtpServer ... In loop. The $i Javascript 'line' is: <<" . $seg . ">>\n");
                    my @jsRecords = split $jsFieldSep, $seg;

                    my $jsValue = $jsRecords[3];

                    # Remove the surrounding "[..]" for the destinations attribute value
                    if ( $i == 6 ) {
                        $jsValue = substr $jsValue, 1, (length $jsValue) -2 ;
                    }
                    #mesg ("smtpServer ... i= $i, the jsValue=<<" . $jsValue . ">>\n");
                    push @jsArray, $jsValue;
                }
                my $jsRecord = join "|", @jsArray;

                mesg ("smtpServer ... Found javascript mail record: \n<<" . $jsRecord . ">>\n");
                # Tack on the TO addresses
                $jsRecord .= '|' . join(",", @{$client->{TO}});
                print( RECORDFILE "$record" . '|' . "$jsRecord" . '||' . "\n");
            }
            else {
                mesg ("--- ERROR: Bad mail: number of javascript records: $#jsSegments not as expected: 7\n");
            }
        }
        elsif ( $command eq "reset" ) {

            my @names  = split $testNameSep, $message;

            if (  $#names != 2 ) {
                mesg ("--- ERROR: Bad reset mail: number of components: $#names not as expected: 2\n");
                next;
            }
            # Close the RECORDFILE if one is in use
            if ( $recordFileName ne '' ) {
                mesg ("smtpServer ... Closing Recordfile: '" . $recordFileName . "'\n");
                close( RECORDFILE);
                sleep 1;
            }
            my $testName = $names[ 1];

            mesg ("smtpServer ... ModTest Name is '" . $testName . "' \n");

            my $recordFileName = $testName . "/" . $gRecfile ;

            open( RECORDFILE, "> $recordFileName") or die "Cannot open $recordFileName file for writing: $!";

            RECORDFILE->autoflush;

            mesg ("smtpServer ... Ready to start recording in $recordFileName record file \n");
        }
        elsif ( $command eq "stop" ) {
            # Received command to stop SMTP server
            mesg ("smtpServer ... Received Command to STOP.... Stopping\n");
            mesg ("smtpServer ... Closing the recordfile $gRecfile\n");
            close( RECORDFILE);
            #close( LOGFILE);
            last;
        }
    }

    mesg ("SMTP ...EXITING ... (test: " . $gTestname . ")\n");
}

main();
