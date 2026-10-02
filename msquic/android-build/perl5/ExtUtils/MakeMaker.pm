package ExtUtils::MakeMaker;
# Minimal stand-in for Git's perl, which ships without MakeMaker: OpenSSL's
# Configure only needs MM->maybe_command, which IPC::Cmd uses to find an
# executable on the PATH (Configure's "which").
use strict;
our $VERSION = '7.70';

package MM;

sub maybe_command {
    my ($self, $file) = @_;
    return $file if -x $file && !-d _;
    return;
}

1;
