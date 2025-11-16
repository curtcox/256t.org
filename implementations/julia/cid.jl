module CID

using SHA
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

export BASE_DIR, EXAMPLES_DIR, CIDS_DIR, compute_cid

end
