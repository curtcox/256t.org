Code.require_file("cid.exs", __DIR__)

files =
  CID.cids_dir()
  |> File.ls!()
  |> Enum.map(&Path.join(CID.cids_dir(), &1))
  |> Enum.reject(&File.dir?/1)
  |> Enum.sort()

mismatches =
  for path <- files,
      content = File.read!(path),
      expected = CID.compute_cid(content),
      actual = Path.basename(path),
      actual != expected do
    {actual, expected}
  end

if mismatches == [] do
  IO.puts("All #{length(files)} CID files match their contents.")
else
  IO.puts("Found CID mismatches:")

  Enum.each(mismatches, fn {actual, expected} ->
    IO.puts("- #{actual} should be #{expected}")
  end)

  System.halt(1)
end
