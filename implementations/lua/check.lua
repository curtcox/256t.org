local script_path = debug.getinfo(1, "S").source:sub(2)
local script_dir = script_path:match("(.+)/") or "."
local base_dir = script_dir .. "/../.."
local cids_dir = base_dir .. "/cids"

local function base64url_encode(data)
  local alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"
  local result = {}
  local len = #data
  local i = 1
  while i <= len do
    local b1 = data:byte(i) or 0
    local b2 = data:byte(i + 1) or 0
    local b3 = data:byte(i + 2) or 0

    local n = (b1 << 16) | (b2 << 8) | b3

    local c1 = (n >> 18) & 0x3F
    local c2 = (n >> 12) & 0x3F
    local c3 = (n >> 6) & 0x3F
    local c4 = n & 0x3F

    result[#result + 1] = alphabet:sub(c1 + 1, c1 + 1)
    result[#result + 1] = alphabet:sub(c2 + 1, c2 + 1)
    if i + 1 <= len then
      result[#result + 1] = alphabet:sub(c3 + 1, c3 + 1)
    end
    if i + 2 <= len then
      result[#result + 1] = alphabet:sub(c4 + 1, c4 + 1)
    end

    i = i + 3
  end

  return table.concat(result)
end

local function encode_length(length)
  local bytes = {}
  for i = 5, 0, -1 do
    bytes[#bytes + 1] = string.char((length >> (8 * i)) & 0xFF)
  end
  return base64url_encode(table.concat(bytes))
end

local function read_file(path)
  local file = io.open(path, "rb")
  if not file then
    return nil
  end
  local content = file:read("*a")
  file:close()
  return content
end

local function sha512(data)
  local tmp = os.tmpname()
  local out = assert(io.open(tmp, "wb"))
  out:write(data)
  out:close()

  local handle = assert(io.popen("openssl dgst -binary -sha512 " .. tmp, "r"))
  local digest = handle:read("*a")
  handle:close()
  os.remove(tmp)

  return digest
end

local function compute_cid(content)
  local prefix = encode_length(#content)
  local suffix

  if #content <= 64 then
    suffix = base64url_encode(content)
  else
    suffix = base64url_encode(sha512(content))
  end

  return prefix .. suffix
end

local function list_entries(path)
  local handle = assert(io.popen("ls -1 '" .. path .. "'"))
  local entries = {}
  for name in handle:lines() do
    entries[#entries + 1] = name
  end
  handle:close()
  table.sort(entries)
  return entries
end

local function main()
  local mismatches = {}
  local count = 0

  for _, name in ipairs(list_entries(cids_dir)) do
    local path = cids_dir .. "/" .. name
    local content = read_file(path)

    if content then
      count = count + 1
      local expected = compute_cid(content)
      if name ~= expected then
        mismatches[#mismatches + 1] = { name = name, expected = expected }
      end
    end
  end

  if #mismatches > 0 then
    print("Found CID mismatches:")
    for _, mismatch in ipairs(mismatches) do
      print("- " .. mismatch.name .. " should be " .. mismatch.expected)
    end
    return 1
  end

  print(string.format("All %d CID files match their contents.", count))
  return 0
end

os.exit(main())
