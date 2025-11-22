# frozen_string_literal: true

require_relative "cid"

def main
  mismatches = []
  download_failures = []
  count = 0
  base_url = "https://256t.org"

  entries = CIDS_DIR.children.select(&:file?).sort_by { |path| path.basename.to_s }

  entries.each do |path|
    count += 1
    cid = path.basename.to_s
    local_content = path.binread
    expected = compute_cid(local_content)
    
    # Check local CID file
    mismatches << [cid, expected] if expected != cid
    
    # Check downloaded content
    begin
      result = download_cid(base_url, cid)
      if !result[:valid]
        download_failures << [cid, result[:computed]]
      elsif result[:content] != local_content
        download_failures << [cid, "content mismatch with local file"]
      end
    rescue => e
      download_failures << [cid, e.message]
    end
  end

  has_errors = false

  unless mismatches.empty?
    puts "Found CID mismatches:"
    mismatches.each do |actual, expected|
      puts "- #{actual} should be #{expected}"
    end
    has_errors = true
  end

  unless download_failures.empty?
    $stderr.puts "Found download validation failures:"
    download_failures.each do |cid, error|
      $stderr.puts "- #{cid}: #{error}"
    end
    has_errors = true
  end

  return 1 if has_errors

  puts "All #{count} CID files match their contents."
  puts "All #{count} downloaded CIDs are valid."
  0
end

exit(main)
