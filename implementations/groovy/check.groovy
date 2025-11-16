#!/usr/bin/env groovy

import java.nio.file.Files
import java.nio.file.Path
import CidUtil

List<Map.Entry<Path, String>> mismatches = []
int count = 0

Files.list(CidUtil.CIDS_DIR)
    .filter { Files.isRegularFile(it) }
    .sorted()
    .forEach { path ->
        count += 1
        byte[] content = Files.readAllBytes(path)
        String expected = CidUtil.computeCid(content)
        String actual = path.fileName.toString()
        if (actual != expected) {
            mismatches.add(Map.entry(path, expected))
        }
    }

if (!mismatches.isEmpty()) {
    println 'Found CID mismatches:'
    mismatches.each { entry ->
        println "- ${entry.key.fileName} should be ${entry.value}"
    }
    System.exit(1)
}

println "All ${count} CID files match their contents."
