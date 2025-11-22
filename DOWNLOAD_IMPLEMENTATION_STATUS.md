# Download Function Implementation Status

## Overview

This document tracks the implementation of download functionality across all 40 language implementations in the 256t.org repository.

## Completion Status: 21/40 (52.5%)

### ✅ Completed Languages (21)

| # | Language | Function Name | HTTP Library | Status |
|---|----------|--------------|--------------|--------|
| 1 | Python | `download_cid()` | urllib | ✅ Complete |
| 2 | JavaScript | `downloadCid()` | https | ✅ Complete |
| 3 | Go | `downloadCID()` | net/http | ✅ Complete |
| 4 | Ruby | `download_cid()` | Net::HTTP | ✅ Complete |
| 5 | Node (ESM) | `downloadCid()` | https | ✅ Complete |
| 6 | Rust | `download_cid()` | reqwest | ✅ Complete |
| 7 | TypeScript | `downloadCid()` | https | ✅ Complete |
| 8 | PHP | `download_cid()` | file_get_contents | ✅ Complete |
| 9 | Perl | `download_cid()` | LWP::UserAgent | ✅ Complete |
| 10 | Deno | `downloadCid()` | fetch API | ✅ Complete |
| 11 | Java | `downloadCid()` | HttpURLConnection | ✅ Complete |
| 12 | C# | `DownloadAsync()` | HttpClient | ✅ Complete |
| 13 | ECMAScript | `downloadCid()` | fetch API | ✅ Complete |
| 14 | Bash | `download_cid()` | curl | ✅ Complete |
| 15 | PowerShell | `Download-Cid` | WebClient | ✅ Complete |
| 16 | Lua | `download_cid()` | curl (popen) | ✅ Complete |
| 17 | Dart | `downloadCid()` | HttpClient | ✅ Complete |
| 18 | Kotlin | `downloadCid()` | HttpURLConnection | ✅ Complete |
| 19 | Scala | `downloadCid()` | HttpURLConnection | ✅ Complete |
| 20 | Julia | `download_cid()` | HTTP.jl | ✅ Complete |
| 21 | R | `download_cid()` | httr | ✅ Complete |

### 🔄 Remaining Languages (19)

#### High Priority (In CI Workflow)
| Language | Suggested Library | Complexity |
|----------|------------------|------------|
| Groovy | HttpURLConnection | Low |
| Haskell | http-conduit | Medium |
| Crystal | HTTP::Client | Low |
| Nim | httpclient | Low |
| OCaml | cohttp | Medium |
| Erlang | httpc | Medium |
| Elixir | HTTPoison/Req | Low |
| Racket | net/url | Medium |
| Clojure | clj-http | Low |
| Prolog | http_open | High |
| Emacs Lisp | url-retrieve | Medium |
| Tcl | http package | Low |
| CMake | file(DOWNLOAD) | Low |
| Zig | std.http.Client | Medium |
| D | std.net.curl | Low |
| Fortran | libcurl | High |

#### Standard Priority
| Language | Suggested Library | Complexity |
|----------|------------------|------------|
| C | libcurl | Medium |
| C++ | libcurl/cpr | Medium |
| F# | HttpClient | Low |
| Swift | URLSession | Low |

## Implementation Pattern

All implementations follow this standard pattern:

```
function download_cid(base_url, cid):
    # Construct URL
    url = base_url.trim_end('/') + '/' + cid
    
    # Download content
    response = http_get(url)
    if response.status != 200:
        throw error("HTTP " + response.status)
    
    content = response.body
    
    # Compute and validate CID
    computed = compute_cid(content)
    is_valid = (computed == cid)
    
    # Return result
    return {
        content: content,
        computed: computed,
        is_valid: is_valid
    }
```

## Check Script Integration

Each check script has been updated to:

1. **Local Validation** (existing)
   - Read each file from `/cids/`
   - Compute CID from content
   - Verify filename matches computed CID

2. **Download Validation** (new)
   - Download content from `https://256t.org/{CID}`
   - Compute CID from downloaded content
   - Verify computed CID matches expected CID
   - Verify downloaded content matches local file

3. **Error Reporting**
   - Report local validation failures to stdout
   - Report download validation failures to stderr
   - Exit with code 1 if any validation fails

## Dependencies Added

- **Rust**: `reqwest = { version = "0.11", features = ["blocking"] }`
- **Julia**: Requires HTTP.jl package
- **R**: Requires httr package
- **ECMAScript**: Requires node-fetch for Node.js environment

## Testing

### Manual Testing (Limited in Sandbox)
Tests in the development sandbox fail due to DNS restrictions (cannot access 256t.org).

### CI Testing (Full Validation)
When tests run in CI with internet access:
- All 78 CID files should be downloadable from https://256t.org/
- Downloaded content should match local files
- Computed CIDs should match expected CIDs

## Implementation Notes

### Network Error Handling
All implementations handle:
- Connection failures
- Timeout errors  
- HTTP error status codes
- Content validation failures

### Timeout Settings
Most implementations use 10-second timeouts for HTTP requests.

### Content Comparison
Downloaded content is byte-compared with local files to ensure integrity.

## Security Considerations

1. **HTTPS Only**: All downloads use HTTPS protocol
2. **CID Validation**: Every download is validated against expected CID
3. **No Arbitrary URLs**: Base URL is hardcoded to `https://256t.org`
4. **Content Verification**: Downloaded content must match local reference

## Next Steps

### For Remaining Implementations

1. **Groovy** - Simple, similar to Java/Kotlin pattern
2. **Crystal** - Similar to Ruby pattern
3. **Elixir/Erlang** - Use standard HTTP libraries
4. **F#** - Similar to C# pattern
5. **Swift** - Use URLSession
6. **C/C++** - Use libcurl
7. **Nim** - Use httpclient module
8. **Others** - Follow language-specific HTTP client patterns

### For Testing

1. Run full CI suite to validate all implementations
2. Monitor for network-related failures
3. Consider adding retry logic for transient failures
4. Consider environment variable to make download validation optional

### For Documentation

1. Update README with download functionality
2. Document any new dependencies
3. Add troubleshooting guide for network issues

## Completion Timeline

- **Current**: 21/40 languages (52.5%)
- **Target**: 40/40 languages (100%)
- **Remaining Effort**: ~19 implementations

Each remaining implementation should take 10-20 minutes following established patterns.
