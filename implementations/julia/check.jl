include("cid.jl")
using .CID

function main()
    entries = sort(readdir(CID.CIDS_DIR))
    mismatches = Vector{Tuple{String, String}}()
    download_failures = Vector{Tuple{String, String}}()
    count = 0
    base_url = "https://256t.org"

    for name in entries
        path = joinpath(CID.CIDS_DIR, name)
        if isdir(path)
            continue
        end

        count += 1
        local_content = read(path)
        expected = CID.compute_cid(local_content)

        # Check local CID file
        if expected != name
            push!(mismatches, (name, expected))
        end

        # Check downloaded content
        try
            result = CID.download_cid(base_url, name)
            if !result.is_valid
                push!(download_failures, (name, result.computed))
            elseif result.content != local_content
                push!(download_failures, (name, "content mismatch with local file"))
            end
        catch e
            push!(download_failures, (name, string(e)))
        end
    end

    has_errors = false

    if !isempty(mismatches)
        println("Found CID mismatches:")
        for (cid, expected) in mismatches
            println("- $cid should be $expected")
        end
        has_errors = true
    end

    if !isempty(download_failures)
        println(stderr, "Found download validation failures:")
        for (cid, error) in download_failures
            println(stderr, "- $cid: $error")
        end
        has_errors = true
    end

    if has_errors
        return 1
    end

    println("All $count CID files match their contents.")
    println("All $count downloaded CIDs are valid.")
    return 0
end

exit(main())
