import java.nio.file.Path
import java.nio.file.Paths
import java.security.MessageDigest
import java.util.Base64

class CidUtil {
    static final Path BASE_DIR = Paths.get('.').toAbsolutePath().normalize()
    static final Path EXAMPLES_DIR = BASE_DIR.resolve('examples')
    static final Path CIDS_DIR = BASE_DIR.resolve('cids')

    private static String toBase64Url(byte[] data) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(data)
    }

    private static byte[] encodeLengthBytes(long length) {
        byte[] bytes = new byte[6]
        for (int i = 0; i < 6; i++) {
            int shift = (5 - i) * 8
            bytes[i] = (byte) ((length >> shift) & 0xFF)
        }
        return bytes
    }

    static String encodeLength(long length) {
        return toBase64Url(encodeLengthBytes(length))
    }

    static String computeCid(byte[] content) {
        String prefix = encodeLength(content.length)
        String suffix
        if (content.length <= 64) {
            suffix = toBase64Url(content)
        } else {
            byte[] hash = MessageDigest.getInstance('SHA-512').digest(content)
            suffix = toBase64Url(hash)
        }
        return prefix + suffix
    }
}
