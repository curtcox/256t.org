defmodule CID do
  @base_dir Path.expand("../..", __DIR__)
  @examples_dir Path.join(@base_dir, "examples")
  @cids_dir Path.join(@base_dir, "cids")

  def base_dir, do: @base_dir
  def examples_dir, do: @examples_dir
  def cids_dir, do: @cids_dir

  defp to_base64url(data) when is_binary(data) do
    Base.url_encode64(data, padding: false)
  end

  defp encode_length(length) when is_integer(length) and length >= 0 do
    <<length::unsigned-big-size(48)>>
    |> to_base64url()
  end

  def compute_cid(content) when is_binary(content) do
    prefix = encode_length(byte_size(content))

    suffix =
      if byte_size(content) <= 64 do
        to_base64url(content)
      else
        :crypto.hash(:sha512, content) |> to_base64url()
      end

    prefix <> suffix
  end
end
