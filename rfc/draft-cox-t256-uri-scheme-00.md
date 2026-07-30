# The 't256' URI Scheme for 256t Content Identifiers

```
Internet-Draft                                                    C. Cox
Intended status: Informational                                  256t.org
Expires: 31 January 2027                                    30 July 2026

           The 't256' URI Scheme for 256t Content Identifiers
                      draft-cox-t256-uri-scheme-00
```

## Abstract

This document defines the 't256' Uniform Resource Identifier (URI)
scheme.  A 't256' URI contains exactly one 256t content identifier
(CID) and nothing else.  A CID names content by its length and either
its SHA-512 hash or, for content of 64 bytes or less, the content
itself.  A 't256' URI therefore identifies content independently of
any location, protocol, or server, and any party holding the content
can verify that it matches the URI.

## Status of This Memo

This Internet-Draft is submitted in full conformance with the
provisions of BCP 78 and BCP 79.

Internet-Drafts are working documents of the Internet Engineering
Task Force (IETF).  Note that other groups may also distribute
working documents as Internet-Drafts.  The list of current Internet-
Drafts is at https://datatracker.ietf.org/drafts/current/.

Internet-Drafts are draft documents valid for a maximum of six months
and may be updated, replaced, or obsoleted by other documents at any
time.  It is inappropriate to use Internet-Drafts as reference
material or to cite them other than as "work in progress."

This Internet-Draft will expire on 31 January 2027.

## Copyright Notice

Copyright (c) 2026 IETF Trust and the persons identified as the
document authors.  All rights reserved.

## 1. Introduction

The 256t specification [256T] defines a simple form of content
addressable storage.  Content is named by a content identifier (CID):
a string of at most 94 characters consisting of an 8-character length
prefix followed by either the base64url encoding of the SHA-512 hash
of the content or, when the content is 64 bytes or less, the
base64url encoding of the content itself.

CIDs are location independent.  The same CID names the same content
regardless of which server, filesystem, or medium holds a copy.
Existing practice embeds CIDs as the final path segment of an
'https' URI, which ties the identifier to one particular host.  A
URI scheme that carries only the CID allows content to be referenced
without naming any host at all, leaving resolution to the consumer.

The natural scheme name would be "256t", matching the specification's
name.  That spelling is not available: the URI syntax [RFC3986]
requires a scheme to begin with a letter.  This document therefore
defines the scheme "t256", the shortest valid spelling that preserves
the name.  Section 6 discusses this and other alternatives that were
considered and rejected.

## 2. Conventions and Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "NOT RECOMMENDED", "MAY", and
"OPTIONAL" in this document are to be interpreted as described in
BCP 14 [RFC2119] [RFC8174] when, and only when, they appear in all
capitals, as shown here.

"base64url" means the URL and filename safe base 64 alphabet of
Section 5 of [RFC4648], used *without* padding characters ("=").

"CID" means a 256t content identifier as defined in Section 3.

"Content" means the sequence of octets that a CID names.

## 3. Scheme Syntax

A 't256' URI is the scheme name, a colon, and a single CID.  The
scheme is opaque: it has no authority component, no path hierarchy,
no query component, and no defined fragment semantics.

In the Augmented Backus-Naur Form (ABNF) of [RFC5234]:

```
t256-URI       = "t256:" cid

cid            = length-prefix payload

length-prefix  = 8base64url-char

payload        = 0*86base64url-char

base64url-char = ALPHA / DIGIT / "-" / "_"
```

The following constraints apply in addition to the ABNF:

1.  The length prefix is the unpadded base64url encoding of the
    content's length in octets, represented as a 6-octet big-endian
    unsigned integer.  Six octets encode to exactly 8 base64url
    characters with no padding.  Let N be the decoded length.

2.  If N is less than or equal to 64, the payload is the unpadded
    base64url encoding of the content itself.  Its decoded value
    MUST be exactly N octets long.

3.  If N is greater than 64, the payload is the unpadded base64url
    encoding of the SHA-512 hash [FIPS180-4] of the content.  It is
    always exactly 86 characters long.

4.  Because unpadded base64url encodings of octet strings never have
    a length congruent to 1 modulo 4, a payload length congruent to
    1 modulo 4 is invalid.

A CID is therefore between 8 characters (empty content) and 94
characters long, and a 't256' URI is between 13 and 99 characters
long.

The scheme name is case-insensitive as required by [RFC3986] and
SHOULD be written in lowercase.  The CID is case-sensitive:
base64url distinguishes uppercase from lowercase, and two CIDs that
differ only in case name different content.  Normalization processes
MUST NOT alter the case of the CID.

Because every character of a CID is in the "unreserved" set of
[RFC3986], a CID never requires percent-encoding, and a 't256' URI
producer MUST NOT percent-encode any part of one.  A consumer
encountering a percent-encoded triplet in a 't256' URI MUST treat
the URI as invalid rather than decode it.

## 4. Scheme Semantics

A 't256' URI names content; it does not name a location.  The
operations available on it are:

Verification:  Given candidate content, a consumer computes the CID
   of that content per Section 3 and compares it, as a case-
   sensitive string, to the CID in the URI.  The content matches
   the URI if and only if the strings are equal.

Direct extraction:  If the decoded length prefix is 64 or less, the
   URI itself contains the content, and a consumer MAY obtain the
   content by base64url-decoding the payload without any retrieval
   step.

Retrieval:  A consumer MAY attempt to obtain the content from any
   256t-compatible server.  Given a base URL, retrieval is an HTTP
   GET of the base URL with the CID appended as the final path
   segment [256T].  How a consumer discovers suitable servers is
   outside the scope of this document.  A consumer that retrieves
   content by any means MUST verify it as described above before
   treating it as the content named by the URI.

Two 't256' URIs are equivalent if and only if their CIDs are
codepoint-for-codepoint identical after case-normalizing the scheme
name.

## 5. Encoding Considerations

The scheme-specific part is restricted to the 64 characters of the
base64url alphabet, all of which are unreserved in [RFC3986] and
representable in 7-bit US-ASCII.  No percent-encoding,
internationalization, or character set considerations arise.  A
't256' URI is identical in its URI form and its IRI form.

## 6. Rejected Alternatives

The following alternative designs were considered:

"256t:":  Matches the specification name exactly but violates the
   scheme grammar of [RFC3986], which requires the first character
   to be a letter.  Standard URI parsers reject it and IANA could
   not register it.

"t256://<cid>":  The authority form places the CID in the host
   position.  Host names are case-insensitive and are lowercased by
   generic URI processing, which destroys a base64url CID.  The
   opaque form avoids this entire class of corruption.

"cid:<cid>":  The 'cid' scheme is already registered for MIME
   Content-ID references [RFC2392] and is not available despite the
   coincidence of terminology.

"ni:" [RFC6920]:  The Named Information scheme also names content by
   hash but has its own incompatible structure, with no place for
   the 256t length prefix or the inline-content rule for small
   content.  A 256t CID cannot be losslessly expressed as an 'ni'
   URI.

"urn:256t:<cid>":  [RFC8141] permits a URN namespace identifier to
   begin with a digit, so this form is syntactically valid and may
   be pursued separately for contexts that require URNs.  It was
   rejected as the primary form because of its verbosity and lack
   of any resolution story in deployed software.

## 7. Interoperability Considerations

The 't256' scheme is opaque, so software that applies generic
hierarchical processing (relative reference resolution, path
merging, dot-segment removal) has nothing to operate on; a 't256'
URI cannot be used as a base URI for relative references.

Applications that display CIDs SHOULD present them verbatim.
Truncation for display is acceptable only when the full URI remains
available to copy, since a truncated CID names nothing.

## 8. Security Considerations

Collision resistance:  The binding between a CID and content of
   more than 64 octets rests on the collision resistance of SHA-512.
   A CID does not, strictly speaking, uniquely determine content;
   it is computationally infeasible, with currently known
   techniques, to produce two contents with the same CID.  For
   content of 64 octets or less the binding is exact, because the
   URI contains the content itself.

Mandatory verification:  It is far easier for a malicious or broken
   server to return wrong content than to engineer a hash collision.
   The verification requirement of Section 4 is therefore MUST-level:
   consumers that skip it lose every integrity property the scheme
   provides and reduce a 't256' URI to an unauthenticated file name.

Confidentiality of small content:  For content of 64 octets or less,
   the URI *is* the content, merely base64url-encoded.  Such URIs
   MUST be treated with the same sensitivity as the content itself
   wherever URIs are logged, cached, displayed, or shared.  Base64url
   is an encoding, not encryption.

Length disclosure:  Every CID discloses the exact length of the
   content, which can itself be revealing (for example,
   distinguishing between a small set of candidate documents).

Availability is unaddressed:  A CID carries no information about
   where, or whether, the content can be retrieved.  Possession of a
   valid 't256' URI is no assurance that any server will produce the
   content.

Cacheability:  Verified content MAY be cached indefinitely under its
   CID.  Caches MUST key on the full CID and MUST verify content
   before inserting it.

## 9. IANA Considerations

IANA is requested to register the following URI scheme in the
"Uniform Resource Identifier (URI) Schemes" registry, per the
guidelines in [RFC7595].

```
Scheme name:                 t256
Status:                      Provisional
Applications/protocols:      256t content addressable storage
                             clients and servers; any application
                             referencing content by 256t CID.
Contact:                     Curt Cox <curtcox@gmail.com>
Change controller:           Curt Cox
References:                  This document; https://256t.org
```

## 10. References

### 10.1. Normative References

- [RFC2119]  Bradner, S., "Key words for use in RFCs to Indicate
  Requirement Levels", BCP 14, RFC 2119, March 1997.
- [RFC3986]  Berners-Lee, T., Fielding, R., and L. Masinter,
  "Uniform Resource Identifier (URI): Generic Syntax", STD 66,
  RFC 3986, January 2005.
- [RFC4648]  Josefsson, S., "The Base16, Base32, and Base64 Data
  Encodings", RFC 4648, October 2006.
- [RFC5234]  Crocker, D. and P. Overell, "Augmented BNF for Syntax
  Specifications: ABNF", STD 68, RFC 5234, January 2008.
- [RFC8174]  Leiba, B., "Ambiguity of Uppercase vs Lowercase in
  RFC 2119 Key Words", BCP 14, RFC 8174, May 2017.
- [FIPS180-4]  National Institute of Standards and Technology,
  "Secure Hash Standard (SHS)", FIPS PUB 180-4, August 2015.
- [256T]  "256t.org Content Addressable Storage Specification",
  https://256t.org.

### 10.2. Informative References

- [RFC2392]  Levinson, E., "Content-ID and Message-ID Uniform
  Resource Locators", RFC 2392, August 1998.
- [RFC6920]  Farrell, S., Kutscher, D., Dannewitz, C., Ohlman, B.,
  Keranen, A., and P. Hallam-Baker, "Naming Things with Hashes",
  RFC 6920, April 2013.
- [RFC7595]  Thaler, D., Hansen, T., and T. Hardie, "Guidelines and
  Registration Procedures for URI Schemes", BCP 35, RFC 7595,
  June 2015.
- [RFC8141]  Saint-Andre, P. and J. Klensin, "Uniform Resource
  Names (URNs)", RFC 8141, April 2017.

## Appendix A. Examples

All examples were produced with the reference implementations at
[256T].

Empty content (0 octets; payload empty):

```
t256:AAAAAAAA
```

The 13-octet string "Hello, World!" (content carried inline; the
payload "SGVsbG8sIFdvcmxkIQ" base64url-decodes to the content):

```
t256:AAAAAAANSGVsbG8sIFdvcmxkIQ
```

65 octets, each the letter "a" (over the 64-octet threshold, so the
payload is the 86-character base64url SHA-512 hash and the URI has
its maximum length of 99 characters):

```
t256:AAAAAABBuDCGzYSU5VcIrX7Ngt-0vKG9ph7Lt8rwxolnkC5wk0Xl2DBet6wNWIr8bLt1FhqpyMfg6phr2DPa_l4czTc0Wg
```

## Author's Address

```
Curt Cox
Email: curtcox@gmail.com
URI:   https://256t.org
```
