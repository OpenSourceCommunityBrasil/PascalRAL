package Pod::Usage;
# Minimal stand-in for Git's perl, which ships without this module: OpenSSL's
# configdata.pm loads it only for its usage message.
use strict;
use Exporter 'import';
our @EXPORT = qw(pod2usage);
our $VERSION = '2.03';

sub pod2usage {
    my %a = @_ == 1 ? (-message => $_[0]) : @_;
    print STDERR (defined $a{-message} ? $a{-message} : "usage: see the source\n");
    exit(defined $a{-exitval} ? $a{-exitval} : 2);
}

1;
