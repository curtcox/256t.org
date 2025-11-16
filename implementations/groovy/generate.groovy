#!/usr/bin/env groovy

import java.nio.file.Files
import java.nio.file.Path

Files.createDirectories(CidUtil.CIDS_DIR)

Files.list(CidUtil.EXAMPLES_DIR)
    .filter { Files.isRegularFile(it) }
    .sorted()
    .forEach { Path example ->
        byte[] content = Files.readAllBytes(example)
        String cid = CidUtil.computeCid(content)
        Path destination = CidUtil.CIDS_DIR.resolve(cid)
        Files.write(destination, content)
        println "Wrote ${destination.fileName} from ${example.fileName}"
    }
