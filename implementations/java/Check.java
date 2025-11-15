import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Check {
    public static void main(String[] args) throws IOException {
        int mismatches = 0;
        int count = 0;
        for (Path path : Files.list(CidUtil.CIDS_DIR).toList()) {
            if (Files.isDirectory(path)) {
                continue;
            }
            count++;
            byte[] content = CidUtil.readFile(path);
            String expected = CidUtil.computeCid(content);
            if (!path.getFileName().toString().equals(expected)) {
                System.out.printf("%s should be %s%n", path.getFileName(), expected);
                mismatches++;
            }
        }
        if (mismatches > 0) {
            System.err.printf("Found %d mismatched CID file(s).%n", mismatches);
            System.exit(1);
        }
        System.out.printf("All %d CID files match their contents.%n", count);
    }
}
