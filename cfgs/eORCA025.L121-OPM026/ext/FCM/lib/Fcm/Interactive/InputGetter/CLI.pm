# ------------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# ------------------------------------------------------------------------------
use strict;
use warnings;

package Fcm::Interactive::InputGetter::CLI;
use base qw{Fcm::Interactive::InputGetter};

my $DEF_MSG = q{ (or just press <return> for "%s")};
my %EXTRA_MSG_FOR = (
    yn  => qq{\nEnter "y" or "n"},
    yna => qq{\nEnter "y", "n" or "a"},
);
my %CHECKER_FOR = (
    yn  => sub {$_[0] eq 'y' || $_[0] eq 'n'},
    yna => sub {$_[0] eq 'y' || $_[0] eq 'n' || $_[0] eq 'a'},
);

sub invoke {
    my ($self) = @_;
    my $type = $self->get_type() ? lc($self->get_type()) : q{};
    my $message
        = $self->get_message()
        . (exists($EXTRA_MSG_FOR{$type}) ? $EXTRA_MSG_FOR{$type} : q{})
        . ($self->get_default() ? sprintf($DEF_MSG, $self->get_default()) : q{})
        . q{: }
        ;
    while (1) {
        print($message);
        my $answer = readline(STDIN);
        chomp($answer);
        if (!$answer && $self->get_default()) {
            $answer = $self->get_default();
        }
        if (!exists($CHECKER_FOR{$type}) || $CHECKER_FOR{$type}->($answer)) {
            return $answer;
        }
    }
    return;
}

1;
__END__

=head1 NAME

Fcm::Interactive::InputGetter::CLI

=head1 SYNOPSIS

    use Fcm::Interactive;
    $answer = Fcm::Interactive::get_input(
        title   => 'My title',
        message => 'Would you like to ...?',
        type    => 'yn',
        default => 'n',
    );

=head1 DESCRIPTION

This is a solid implementation of
L<Fcm::Interactive::InputGetter|Fcm::Interactive::InputGetter>. It gets a user
reply from STDIN using a prompt on STDOUT.

=head1 METHODS

See L<Fcm::Interactive::InputGetter|Fcm::Interactive::InputGetter> for a list of
methods.

=head1 TO DO

Use IO::Prompt.

=head1 SEE ALSO

L<Fcm::Interactive|Fcm::Interactive>,
L<Fcm::Interactive::InputGetter|Fcm::Interactive::InputGetter>,
L<Fcm::Interactive::InputGetter::GUI|Fcm::Interactive::InputGetter::GUI>

=head1 COPYRIGHT

E<169> Crown copyright Met Office. All rights reserved.

=cut
