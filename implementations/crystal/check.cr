require "digest/sha512"
require "base64"
require "file_utils"

BASE_DIR = File.expand_path(File.join(__DIR__, "..", ".."))
CIDS_DIR = File.join(BASE_DIR, "cids")

# Encode a byte slice using URL-safe Base64 without padding.
def to_base64url(data : Bytes) : String
  Base64.urlsafe_encode(data, padding: false)
end

# Encode the content length into a 6-byte big-endian Base64URL string.
def encode_length(length : Int) : String
  bytes = Bytes.new(6)
  value = length
  5.downto(0) do |index|
    bytes[index] = (value & 0xFF).to_u8
    value >>= 8
  end
  to_base64url(bytes)
end

# Compute the CID for the given content according to the 256t specification.
def compute_cid(content : Bytes) : String
  prefix = encode_length(content.size)
  suffix = if content.size <= 64
             to_base64url(content)
           else
             digest = Digest::SHA512.digest(content)
             to_base64url(digest)
           end
  prefix + suffix
end

mismatches = [] of {String, String}
count = 0

Dir.children(CIDS_DIR).sort.each do |entry|
  path = File.join(CIDS_DIR, entry)
  next if File.directory?(path)

  count += 1
  actual = entry
  # Read file contents as raw bytes to avoid any encoding transformations.
  expected = compute_cid(File.read(path, encoding: "ASCII-8BIT").to_slice)
  mismatches << {actual, expected} if actual != expected
end

if mismatches.empty?
  puts "All #{count} CID files match their contents."
  exit 0
else
  puts "Found CID mismatches:"
  mismatches.each do |actual, expected|
    puts "- #{actual} should be #{expected}"
  end
  exit 1
end
