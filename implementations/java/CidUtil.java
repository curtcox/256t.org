import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

public final class CidUtil {
    private CidUtil() {}

    public static final Path BASE_DIR = Paths.get("").toAbsolutePath();
    public static final Path EXAMPLES_DIR = BASE_DIR.resolve("examples");
    public static final Path CIDS_DIR = BASE_DIR.resolve("cids");

    private static String toBase64Url(byte[] data) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(data);
    }

    public static String encodeLength(long length) {
        byte[] bytes = new byte[6];
        for (int i = 5; i >= 0; i--) {
            bytes[i] = (byte) (length & 0xFF);
            length >>= 8;
        }
        return toBase64Url(bytes);
    }

    public static String computeCid(byte[] content) {
        String prefix = encodeLength(content.length);
        if (content.length <= 64) {
            return prefix + toBase64Url(content);
        }
        try {
            MessageDigest sha512 = MessageDigest.getInstance("SHA-512");
            byte[] hash = sha512.digest(content);
            return prefix + toBase64Url(hash);
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-512 not available", e);
        }
    }

    public static byte[] readFile(Path path) throws IOException {
        return Files.readAllBytes(path);
    }

    public static void writeFile(Path path, byte[] content) throws IOException {
        Files.createDirectories(path.getParent());
        Files.write(path, content);
    }

    public static class DownloadResult {
        public final byte[] content;
        public final String computed;
        public final boolean isValid;

        public DownloadResult(byte[] content, String computed, boolean isValid) {
            this.content = content;
            this.computed = computed;
            this.isValid = isValid;
        }
    }

    public static DownloadResult downloadCid(String baseUrl, String cid) throws IOException {
        if (baseUrl.endsWith("/")) {
            baseUrl = baseUrl.substring(0, baseUrl.length() - 1);
        }
        String url = baseUrl + "/" + cid;
        
        java.net.HttpURLConnection conn = (java.net.HttpURLConnection) 
            new java.net.URL(url).openConnection();
        conn.setRequestMethod("GET");
        conn.setConnectTimeout(10000);
        conn.setReadTimeout(10000);
        
        int responseCode = conn.getResponseCode();
        if (responseCode != 200) {
            throw new IOException("HTTP " + responseCode + ": " + conn.getResponseMessage());
        }
        
        byte[] content = conn.getInputStream().readAllBytes();
        String computed = computeCid(content);
        boolean isValid = computed.equals(cid);
        
        return new DownloadResult(content, computed, isValid);
    }
}
