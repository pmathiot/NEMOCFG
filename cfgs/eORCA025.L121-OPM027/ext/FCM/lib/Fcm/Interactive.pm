# ------------------------------------------------------------------------------
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT.txt
# which you should have received as part of this distribution.
# ------------------------------------------------------------------------------
use strict;
use warnings;

package Fcm::Interactive;
use base qw{Exporter};

our @EXPORT_OK = qw{get_input};

use Fcm::Util::ClassLoader;

my $DEFAULT_IMPL_CLASS = 'Fcm::Interactive::InputGetter::CLI';
my %DEFAULT_IMPL_CLASS_OPTIONS = ();

my $IMPL_CLASS = $DEFAULT_IMPL_CLASS;
my %IMPL_CLASS_OPTIONS = %DEFAULT_IMPL_CLASS_OPTIONS;

################################################################################
# Returns the name of the current class/settings for getting input
sub get_impl {
    return (wantarray() ? ($IMPL_CLASS, \%IMPL_CLASS_OPTIONS) : $IMPL_CLASS);
}

################################################################################
# Returns the name of the current class/settings for getting input
sub get_default_impl {
    return (
        wantarray()
        ? ($DEFAULT_IMPL_CLASS, \%DEFAULT_IMPL_CLASS_OPTIONS)
        : $DEFAULT_IMPL_CLASS
    );
}

################################################################################
# Sets the name of the class/settings for getting input
sub set_impl {
    my ($impl_class, $impl_class_options_ref) = @_;
    if ($impl_class) {
        $IMPL_CLASS = $impl_class;
        if ($impl_class_options_ref) {
            %IMPL_CLASS_OPTIONS = (%{$impl_class_options_ref});
        }
        else {
            %IMPL_CLASS_OPTIONS = ();
        }
    }
}

################################################################################
# Gets an input from the user and returns it
sub get_input {
    my (%options) = @_;
    my ($class_name, $class_options_ref) = get_impl();
    Fcm::Util::ClassLoader::load($class_name);
    %options = map {lc($_), $options{$_}} keys(%options);
    return $class_name->new({%{$class_options_ref}, %options})->invoke();
}

1;
__END__

=head1 NAME

Fcm::Interactive

=head1 SYNOPSIS

    use Fcm::Interactive;
    Fcm::Interactive::set_impl('My::InputGetter', {option1 => 'value1', ...});
    $answer = Fcm::Interactive::get_input(
        title   => 'My title',
        message => 'Would you like to ...?',
        type    => 'yn',
        default => 'n',
    );

=head1 DESCRIPTION

Common interface for getting an interactive user reply. The default is to use a
L<Fcm::Interactive::InputGetter::CLI|Fcm::Interactive::InputGetter::CLI> object
with no extra options.

=head1 FUNCTIONS

=over 4

=item get_impl()

Returns the class that implements the function for get_input(%options). In
scalar context, returns the class name only. In list context, returns the class
name and the extra hash options that would be passed to its constructor.

=item get_default_impl()

Returns the defaut values for get_impl().

=item set_impl($impl_class,$impl_class_options_ref)

Sets the class that implements the function for get_input(%options). The name
of the class is given in $impl_class. Any extra options that should be given to
the constructor should be set in the hash reference $impl_class_options_ref.

=item get_input(%options)

Calls the appropriate function to get an input string from the user, and
returns it.

Input options are: I<title>, for a short title of the prompt, I<message>, for
the message prompt, I<type> for the prompt type, and I<default> for the default
value of the return value.

Prompt type can be YN (yes or no), YNA (yes, no or all) or input (for an input
string).

=back

=head1 SEE ALSO

L<Fcm::Interactive::InputGetter|Fcm::Interactive::InputGetter>,
L<Fcm::Interactive::InputGetter::CLI|Fcm::Interactive::InputGetter::CLI>,
L<Fcm::Interactive::InputGetter::GUI|Fcm::Interactive::InputGetter::GUI>

=head1 COPYRIGHT

E<169> Crown copyright Met Office. All rights reserved.

=cut
