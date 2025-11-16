include("cid.jl")
using .CID

function main()
    ispath(CID.CIDS_DIR) || mkpath(CID.CIDS_DIR)

    for name in sort(readdir(CID.EXAMPLES_DIR))
        path = joinpath(CID.EXAMPLES_DIR, name)
        if isdir(path)
            continue
        end

        content = read(path)
        cid = CID.compute_cid(content)
        destination = joinpath(CID.CIDS_DIR, cid)
        write(destination, content)
        println("Wrote $(basename(destination)) from $name")
    end

    return 0
end

exit(main())
