Code.require_file("cid.exs", __DIR__)

File.mkdir_p!(CID.cids_dir())

CID.examples_dir()
|> File.ls!()
|> Enum.map(&Path.join(CID.examples_dir(), &1))
|> Enum.reject(&File.dir?/1)
|> Enum.sort()
|> Enum.each(fn example ->
  content = File.read!(example)
  cid = CID.compute_cid(content)
  destination = Path.join(CID.cids_dir(), cid)
  File.write!(destination, content)
  IO.puts("Wrote #{Path.basename(destination)} from #{Path.basename(example)}")
end)
