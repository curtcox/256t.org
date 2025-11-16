# frozen_string_literal: true

require "base64"
require "digest"
require "pathname"

BASE_DIR = Pathname(__dir__).join("..", "..").realpath
EXAMPLES_DIR = BASE_DIR.join("examples")
CIDS_DIR = BASE_DIR.join("cids")

def to_base64url(data)
  Base64.urlsafe_encode64(data).delete("=")
end

def encode_length(length)
  bytes = [length].pack("Q>")
  to_base64url(bytes[-6, 6])
end

def compute_cid(content)
  prefix = encode_length(content.bytesize)
  suffix = if content.bytesize <= 64
    to_base64url(content)
  else
    to_base64url(Digest::SHA512.digest(content))
  end
  prefix + suffix
end

