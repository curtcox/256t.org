import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.Base64
import kotlin.system.exitProcess

private val baseDir: Path = Paths.get("").toAbsolutePath().normalize()
private val cidsDir: Path = baseDir.resolve("cids")

private fun toBase64Url(data: ByteArray): String =
    Base64.getUrlEncoder().withoutPadding().encodeToString(data)

private fun encodeLength(length: Long): String {
    val buffer = ByteArray(6)
    for (i in 0 until 6) {
        buffer[5 - i] = ((length shr (8 * i)) and 0xFF).toByte()
    }
    return toBase64Url(buffer)
}

private fun computeCid(content: ByteArray): String {
    val prefix = encodeLength(content.size.toLong())
    val suffix = if (content.size <= 64) {
        toBase64Url(content)
    } else {
        val digest = MessageDigest.getInstance("SHA-512").digest(content)
        toBase64Url(digest)
    }
    return prefix + suffix
}

fun main() {
    val mismatches = mutableListOf<String>()
    var count = 0
    Files.list(cidsDir).sorted().forEach { path ->
        if (Files.isDirectory(path)) return@forEach
        count += 1
        val actual = path.fileName.toString()
        val expected = computeCid(Files.readAllBytes(path))
        if (actual != expected) {
            mismatches.add("${path.fileName} should be $expected")
        }
    }

    if (mismatches.isNotEmpty()) {
        println("Found CID mismatches:")
        mismatches.forEach { println("- $it") }
        exitProcess(1)
    }
    println("All $count CID files match their contents.")
}
