#!/usr/bin/env perl
use strict;
use warnings;
use Digest::SHA qw(sha512);
use File::Basename qw(dirname);
use File::Spec;
use Cwd qw(abs_path);
use MIME::Base64 qw(encode_base64);

sub base_dir {
    my $script_dir = abs_path(dirname(__FILE__));
    return abs_path(File::Spec->catdir($script_dir, File::Spec->updir, File::Spec->updir));
}

sub cids_dir {
    return File::Spec->catdir(base_dir(), 'cids');
}

sub to_base64url {
    my ($data) = @_;
    my $encoded = encode_base64($data, '');
    $encoded =~ tr{+/}{-_};
    $encoded =~ s/=+$//;
    return $encoded;
}

sub encode_length {
    my ($length) = @_;
    my $bytes = pack('Q>', $length);
    return to_base64url(substr($bytes, 2));
}

sub compute_cid {
    my ($content) = @_;
    my $prefix = encode_length(length($content));
    my $suffix;
    if (length($content) <= 64) {
        $suffix = to_base64url($content);
    }
    else {
        $suffix = to_base64url(sha512($content));
    }
    return $prefix . $suffix;
}

sub read_file_bytes {
    my ($path) = @_;
    open my $fh, '<:raw', $path or die "Unable to read $path: $!";
    local $/;
    return <$fh>;
}

sub main {
    my $dir = cids_dir();
    opendir my $dh, $dir or die "Unable to open $dir: $!";
    my @files = sort grep { -f File::Spec->catfile($dir, $_) } readdir $dh;
    closedir $dh;

    my @mismatches;
    my $count = 0;

    for my $file (@files) {
        $count++;
        my $path = File::Spec->catfile($dir, $file);
        my $content = read_file_bytes($path);
        my $expected = compute_cid($content);
        if ($file ne $expected) {
            push @mismatches, [$file, $expected];
        }
    }

    if (@mismatches) {
        print "Found CID mismatches:\n";
        for my $mismatch (@mismatches) {
            my ($actual, $expected) = @$mismatch;
            print "- $actual should be $expected\n";
        }
        return 1;
    }

    print "All $count CID files match their contents.\n";
    return 0;
}

exit main();
