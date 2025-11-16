include("cid.jl")
using .CID

function main()
    entries = sort(readdir(CID.CIDS_DIR))
    mismatches = Vector{Tuple{String, String}}()
    count = 0

    for name in entries
        path = joinpath(CID.CIDS_DIR, name)
        if isdir(path)
            continue
        end

        count += 1
        content = read(path)
        expected = CID.compute_cid(content)

        if expected != name
            push!(mismatches, (name, expected))
        end
    end

    if !isempty(mismatches)
        println("Found CID mismatches:")
        for (actual, expected) in mismatches
            println("- $actual should be $expected")
        end
        return 1
    end

    println("All $count CID files match their contents.")
    return 0
end

exit(main())
