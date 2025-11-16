# frozen_string_literal: true

require_relative "cid"

def main
  mismatches = []
  count = 0

  entries = CIDS_DIR.children.select(&:file?).sort_by { |path| path.basename.to_s }

  entries.each do |path|
    count += 1
    content = path.binread
    expected = compute_cid(content)
    actual = path.basename.to_s
    mismatches << [actual, expected] if expected != actual
  end

  if mismatches.empty?
    puts "All #{count} CID files match their contents."
    return 0
  end

  puts "Found CID mismatches:"
  mismatches.each do |actual, expected|
    puts "- #{actual} should be #{expected}"
  end
  1
end

exit(main)
