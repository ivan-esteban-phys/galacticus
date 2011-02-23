#!/usr/bin/env perl
use lib "./perl";
use System::Redirect;
use Date::Format;
use XML::Simple;
use MIME::Lite;
use Net::SMTP::SSL;
use Data::Dumper;
use File::Slurp qw( slurp );
use File::Find;
use Switch;
use Term::ReadKey;
use Net::DBus;

# Run a suite of tests on the Galacticus code.
# Andrew Benson (19-Aug-2010).

# Read in any configuration options.
if ( -e "galacticusConfig.xml" ) {
    $xml = new XML::Simple;
    $config = $xml->XMLin("galacticusConfig.xml");
}

# Identify e-mail options for this host.
if ( exists($config->{'email'}->{'host'}->{$ENV{'HOST'}}) ) {
    $emailConfig = $config->{'email'}->{'host'}->{$ENV{'HOST'}};
} elsif ( exists($config->{'email'}->{'host'}->{'default'}) ) {
    $emailConfig = $config->{'email'}->{'host'}->{'default'};
} else {
    $emailConfig->{'method'} = "sendmail";
}
if ( $emailConfig->{'method'} eq "smtp" && exists($emailConfig->{'passwordFrom'}) ) {
    # Get any password now.
    switch ( $emailConfig->{'passwordFrom'} ) {
	case ( "input" ) {
	    print "Please enter your e-mail SMTP password:\n";
	    $smtpPassword = &getPassword;
	}
	case ( "kdewallet" ) {
	    $appName          = "Galacticus";
	    $folderName       = "glc-test-all";
	    my $bus           = Net::DBus->find;
	    my $walletService = $bus->get_service("org.kde.kwalletd");
	    my $walletObject  = $walletService->get_object("/modules/kwalletd");
	    my $walletID      = $walletObject->open("kdewallet",0,$appName);
	    if ( $walletObject->hasEntry($walletID,$folderName,"smtpPassword",$appName) == 1 ) {
		$smtpPassword = $walletObject->readPassword($walletID,$folderName,"smtpPassword",$appName); 
	    } else {
		print "Please enter your e-mail SMTP password:\n";
		$smtpPassword = &getPassword;
		$walletObject->writePassword($walletID,$folderName,"smtpPassword",$smtpPassword,$appName); 
	    }
	}
    }
}

# Open a log file.
$logFile = "testSuite/allTests.log";
open(lHndl,">".$logFile);

# Create a directory for test suite outputs.
system("rm -rf testSuite/outputs");
system("mkdir -p testSuite/outputs");

# Write header to log file.
print lHndl ":-> Running test suite:\n";
print lHndl "    -> Host:\t".$ENV{'HOST'}."\n";
print lHndl "    -> Time:\t".time2str("%a %b %e %T (%Z) %Y", time)."\n";

# Define a list of executables to run.
@executablesToRun = (
    "tests.IO.HDF5.exe",                         # Tests of HDF5 IO routines.
    "tests.array_monotonicity.exe",              # Tests of array monotonicity checking function.
    "tests.cosmic_age.dark_energy.exe",          # Tests of cosmic age calculations.
    "tests.cosmic_age.EdS.exe",                  # .
    "tests.cosmic_age.open.exe",                 # .
    "tests.linear_growth.dark_energy.exe",       # Tests of linear growth factor.
    "tests.linear_growth.EdS.exe",               # .
    "tests.linear_growth.open.exe",              # .
    "tests.comoving_distance.dark_energy.exe",   # Tests of comoving distance calculations.
    "tests.comoving_distance.EdS.exe",           # .
    "tests.comoving_distance.open.exe",          # .
    "tests.Zhao2009_algorithms.dark_energy.exe", # Tests of Zhao et al. (2009) algorithms.
    "tests.Zhao2009_algorithms.EdS.exe",         # .
    "tests.Zhao2009_algorithms.open.exe",        # .
    );

# Run all executables.
foreach $executable ( @executablesToRun ) {
    print lHndl "\n\n";
    print lHndl ":-> Running test: ".$executable."\n";
    &SystemRedirect::tofile("make ".$executable,"allTestsBuild.tmp");
    $buildSuccess = $?;
    print lHndl slurp("allTestsBuild.tmp");
    unlink("allTestsBuild.tmp");
    if ( $buildSuccess == 0 ) {
	# Run the test and copy any output to our log file.
	&SystemRedirect::tofile($executable,"allTests.tmp");
	print lHndl slurp("allTests.tmp");
	unlink("allTests.tmp",$executable);
    } else {
	# Build failed, report an error in the log file.
	print lHndl "FAILED: building ".$executable." failed\n";
    }
}

# Build Galacticus itself.
print lHndl "\n\n";
print lHndl ":-> Building Galacticus...\n";
&SystemRedirect::tofile("make Galacticus.exe","allTestsBuild.tmp");
$buildSuccess = $?;
print lHndl slurp("allTestsBuild.tmp");
unlink("allTestsBuild.tmp");
if ( $buildSuccess == 0 ) {
    # Run all tests.
    @testDirs = ( "testSuite" );
    find(\&runTestScript,@testDirs);
} else {
    # Build failed, report an error in the log file.
    print lHndl "FAILED: building Galacticus.exe failed\n";
}

# Close the log file.
close(lHndl);

# Scan the log file for FAILED.
$lineNumber = 0;
open(lHndl,$logFile);
while ( $line = <lHndl> ) {
    ++$lineNumber;
    if ( $line =~ m/FAILED/ ) {
	push(@failLines,$lineNumber);
    }
}
close(lHndl);
open(lHndl,">>".$logFile);
$emailSubject = "Galacticus test suite log";
if ( $#failLines == -1 ) {
    print lHndl "\n\n:-> All tests were successful.\n";
    print       "All tests were successful.\n";
    $emailSubject .= " [success]";
} else {
    print lHndl "\n\n:-> Failures found. See following lines in log file:\n\t".join("\n\t",@failLines)."\n";
    print $hasFailures." failure(s) found - see ".$logFile." for details.\n";
    $emailSubject .= " [FAILURE]";
}
close(lHndl);

# If we have an e-mail address to send the log to, then do so.
if ( $config->{'contact'}->{'email'} =~ m/\@/ ) {
    # Get e-mail configuration.
    $sendMethod = $emailConfig->{'method'};
    # Construct the message.
    $message  = "Galacticus test suite log is attached.\n";
    $msg = MIME::Lite->new(
	From    => '',
	To      => $config->{'contact'}->{'email'},
	Subject => $emailSubject,
	Type    => 'TEXT',
	Data    => $message
	);
    system("bzip2 -f ".$logFile);
    $msg->attach(
	Type     => "application/x-bzip",
	Path     => $logFile.".bz2",
	Filename => "allTests.log.bz2"
	);

    switch ( $sendMethod ) {
	case ( "sendmail" ) {
	    $msg->send;
	}
	case ( "smtp" ) {
	    my $smtp; 
	    $smtp = Net::SMTP::SSL->new($config->{'email'}->{'host'}, Port=>465) or die "Can't connect";
	    $smtp->auth($config->{'email'}->{'user'},$smtpPassword) or die "Can't authenticate:".$smtp->message();
	    $smtp->mail( $config->{'contact'}->{'email'}) or die "Error:".$smtp->message();
	    $smtp->to( $config->{'contact'}->{'email'}) or die "Error:".$smtp->message();
	    $smtp->data() or die "Error:".$smtp->message();
	    $smtp->datasend($msg->as_string) or die "Error:".$smtp->message();
	    $smtp->dataend() or die "Error:".$smtp->message();
	    $smtp->quit() or die "Error:".$smtp->message();
	}
    }
}

exit;

sub runTestScript {
    # Run a test script.
    $fileName = $_;
    chomp($fileName);

    # Test if this is a script to run.
    if ( $fileName =~ m/^test\-.*\.pl$/ && $fileName ne "test-all.pl" ) {
	print lHndl "\n\n:-> Running test script: ".$fileName."\n";
	&SystemRedirect::tofile($fileName,"allTests.tmp");
	print lHndl slurp("allTests.tmp");
	unlink("allTests.tmp");
    }
}

sub getPassword {
    # Read a password from standard input while echoing asterisks to the screen.
    ReadMode('noecho');
    ReadMode('raw');
    my $password = '';
    while (1) {
	my $c;
	1 until defined($c = ReadKey(-1));
	last if $c eq "\n";
	print "*";
	$password .= $c;
    }
    ReadMode('restore');
    print "\n";
    return $password;
}

