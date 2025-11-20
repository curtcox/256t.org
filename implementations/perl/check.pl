#!/usr/bin/env perl
use strict;
use warnings;
use Digest::SHA qw(sha512);
use File::Basename qw(dirname);
use File::Spec;
use Cwd qw(abs_path);
use MIME::Base64 qw(encode_base64);
use LWP::UserAgent;

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

sub download_cid {
    my ($base_url, $cid) = @_;
    $base_url =~ s{/$}{};
    my $url = "$base_url/$cid";
    
    my $ua = LWP::UserAgent->new(timeout => 10);
    my $response = $ua->get($url);
    
    if (!$response->is_success) {
        die $response->status_line;
    }
    
    my $content = $response->content;
    my $computed = compute_cid($content);
    my $is_valid = $computed eq $cid;
    
    return {
        content => $content,
        computed => $computed,
        is_valid => $is_valid
    };
}

sub main {
    my $dir = cids_dir();
    opendir my $dh, $dir or die "Unable to open $dir: $!";
    my @files = sort grep { -f File::Spec->catfile($dir, $_) } readdir $dh;
    closedir $dh;

    my @mismatches;
    my @download_failures;
    my $count = 0;
    my $base_url = 'https://256t.org';

    for my $file (@files) {
        $count++;
        my $path = File::Spec->catfile($dir, $file);
        my $local_content = read_file_bytes($path);
        my $expected = compute_cid($local_content);
        if ($file ne $expected) {
            push @mismatches, [$file, $expected];
        }
        
        # Check downloaded content
        eval {
            my $result = download_cid($base_url, $file);
            if (!$result->{is_valid}) {
                push @download_failures, [$file, $result->{computed}];
            } elsif ($result->{content} ne $local_content) {
                push @download_failures, [$file, 'content mismatch with local file'];
            }
        };
        if ($@) {
            my $error = $@;
            chomp $error;
            push @download_failures, [$file, $error];
        }
    }

    my $has_errors = 0;

    if (@mismatches) {
        print "Found CID mismatches:\n";
        for my $mismatch (@mismatches) {
            my ($actual, $expected) = @$mismatch;
            print "- $actual should be $expected\n";
        }
        $has_errors = 1;
    }

    if (@download_failures) {
        print STDERR "Found download validation failures:\n";
        for my $failure (@download_failures) {
            my ($cid, $error) = @$failure;
            print STDERR "- $cid: $error\n";
        }
        $has_errors = 1;
    }

    if ($has_errors) {
        return 1;
    }

    print "All $count CID files match their contents.\n";
    print "All $count downloaded CIDs are valid.\n";
    return 0;
}

exit main();
