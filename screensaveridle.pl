#!/usr/bin/env perl

use strict;
use warnings;

use Term::ReadKey;

my $cmd = "dbus-monitor --session \"type='signal',interface='org.gnome.ScreenSaver',member='ActiveChanged'\"";

open (IN, "$cmd |");

ReadMode 'cbreak';    # return keypress immediately without waiting for "Enter"

while (<IN>) {
    if (m/^\s+boolean true/) {
        print "*** Screensaver is active ***\n";
        print "*** Sleeping before megadeath....\n";

        my $key = ReadKey 900;    # timeout after 900 seconds = 15 minutes
        if (defined $key) {
            print "*** A key was pressed; megadeath averted\n";
        } else {
            print "*** killing all jla processes on anvil...\n";
            my $result = `ssh anvil pkill -u jla`;
            print "*** should all be dead\n";
            print $result;
        }
    } elsif (m/^\s+boolean false/) {
        print "*** Screensaver is no longer active ***\n";
    }
}

ReadMode 'restore';    # back to normal input mode

