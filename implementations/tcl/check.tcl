#!/usr/bin/env tclsh

proc to_base64url {data} {
    set standard [binary encode base64 $data]
    set urlsafe [string map {+ - / _} $standard]
    return [string trimright $urlsafe "="]
}

proc encode_length {length} {
    set bytes [binary format "cccccc" \
        [expr {($length >> 40) & 0xFF}] \
        [expr {($length >> 32) & 0xFF}] \
        [expr {($length >> 24) & 0xFF}] \
        [expr {($length >> 16) & 0xFF}] \
        [expr {($length >> 8) & 0xFF}] \
        [expr {$length & 0xFF}]]
    return [to_base64url $bytes]
}

proc sha512_bytes {path} {
    set output [exec sha512sum $path]
    set hex [lindex [split $output " "] 0]
    return [binary format H* $hex]
}

proc compute_cid {path content} {
    set length [string length $content]
    set prefix [encode_length $length]
    if {$length <= 64} {
        set suffix [to_base64url $content]
    } else {
        set hash [sha512_bytes $path]
        set suffix [to_base64url $hash]
    }
    return "$prefix$suffix"
}

set base_dir [file normalize [file join [file dirname [info script]] .. ..]]
set cids_dir [file join $base_dir cids]

set mismatches {}
set count 0
foreach path [lsort [glob -nocomplain -directory $cids_dir *]] {
    if {[file isdirectory $path]} {
        continue
    }
    incr count
    set fh [open $path r]
    fconfigure $fh -translation binary -encoding binary
    set content [read $fh]
    close $fh

    set expected [compute_cid $path $content]
    set actual [file tail $path]
    if {$actual ne $expected} {
        lappend mismatches [list $actual $expected]
    }
}

if {[llength $mismatches] > 0} {
    puts "Found CID mismatches:"
    foreach mismatch $mismatches {
        lassign $mismatch actual expected
        puts "- $actual should be $expected"
    }
    exit 1
} else {
    puts "All $count CID files match their contents."
    exit 0
}
