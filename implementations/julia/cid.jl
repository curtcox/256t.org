module CID

using SHA
using HTTP
import Base64

const BASE_DIR = normpath(joinpath(@__DIR__, "..", ".."))
const EXAMPLES_DIR = joinpath(BASE_DIR, "examples")
const CIDS_DIR = joinpath(BASE_DIR, "cids")

function to_base64url(data::Vector{UInt8})
    encoded = Base64.base64encode(data)
    encoded = replace(encoded, "+" => "-", "/" => "_", "=" => "")
    return encoded
end

function encode_length(length::Int)
    bytes = Vector{UInt8}(undef, 6)
    for i in 0:5
        shift = (5 - i) * 8
        bytes[i + 1] = UInt8((length >> shift) & 0xff)
    end
    return to_base64url(bytes)
end

function compute_cid(content::Vector{UInt8})
    prefix = encode_length(length(content))
    suffix = length(content) <= 64 ? to_base64url(content) : to_base64url(SHA.sha512(content))
    return string(prefix, suffix)
end

struct DownloadResult
    content::Vector{UInt8}
    computed::String
    is_valid::Bool
end

function download_cid(base_url::String, cid::String)
    url = string(rstrip(base_url, '/'), "/", cid)
    
    try
        response = HTTP.get(url)
        if response.status != 200
            error("HTTP $(response.status)")
        end
        
        content = response.body
        computed = compute_cid(content)
        is_valid = computed == cid
        
        return DownloadResult(content, computed, is_valid)
    catch e
        rethrow(e)
    end
end

export BASE_DIR, EXAMPLES_DIR, CIDS_DIR, compute_cid, download_cid

end
