#!/usr/bin/perl

my $cmd = "dbus-monitor --session \"type='signal',interface='org.gnome.ScreenSaver',member='ActiveChanged'\"";

open (IN, "$cmd |");

while (<IN>) {
    if (m/^\s+boolean true/) {
        print "*** Screensaver is active ***\n";
        print "*** Sleeping before megadeath....\n";
        sleep(15*60);
        print "*** killing all jla processes on anvil...\n";
        $result = `ssh anvil pkill -u jla`;
        print "*** should all be dead\n";
        print $result;
            
    } elsif (m/^\s+boolean false/) {
        print "*** Screensaver is no longer active ***\n";
    }
}
