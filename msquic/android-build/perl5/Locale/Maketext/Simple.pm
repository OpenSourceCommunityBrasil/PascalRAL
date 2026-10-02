package Locale::Maketext::Simple;
# Minimal stand-in for Git's perl, which ships without this module: OpenSSL's
# Configure loads it through IPC::Cmd -> Params::Check, only to format
# messages. It does what the original does without Locale::Maketext::Lexicon:
# replaces %1, %2... with the arguments.
use strict;
our $VERSION = '0.21';

sub import {
    my ($class, %args) = @_;
    my $caller = caller;
    no strict 'refs';
    *{"${caller}::loc"} = sub {
        my $s = shift;
        $s =~ s/%(\d+)/defined $_[$1 - 1] ? $_[$1 - 1] : ''/ge;
        return $s;
    };
    *{"${caller}::loc_lang"} = sub { 1 };
}

1;
