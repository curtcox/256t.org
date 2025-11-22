import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Check {
    public static void main(String[] args) throws IOException {
        List<String[]> mismatches = new ArrayList<>();
        List<String[]> downloadFailures = new ArrayList<>();
        int count = 0;
        String baseUrl = "https://256t.org";

        for (Path path : Files.list(CidUtil.CIDS_DIR).sorted().toList()) {
            if (Files.isDirectory(path)) {
                continue;
            }
            count++;
            String cid = path.getFileName().toString();
            byte[] localContent = CidUtil.readFile(path);
            String expected = CidUtil.computeCid(localContent);
            
            // Check local CID file
            if (!cid.equals(expected)) {
                mismatches.add(new String[]{cid, expected});
            }
            
            // Check downloaded content
            try {
                CidUtil.DownloadResult result = CidUtil.downloadCid(baseUrl, cid);
                if (!result.isValid) {
                    downloadFailures.add(new String[]{cid, result.computed});
                } else if (!Arrays.equals(result.content, localContent)) {
                    downloadFailures.add(new String[]{cid, "content mismatch with local file"});
                }
            } catch (Exception e) {
                downloadFailures.add(new String[]{cid, e.getMessage()});
            }
        }

        boolean hasErrors = false;

        if (!mismatches.isEmpty()) {
            System.out.println("Found CID mismatches:");
            for (String[] mismatch : mismatches) {
                System.out.printf("- %s should be %s%n", mismatch[0], mismatch[1]);
            }
            hasErrors = true;
        }

        if (!downloadFailures.isEmpty()) {
            System.err.println("Found download validation failures:");
            for (String[] failure : downloadFailures) {
                System.err.printf("- %s: %s%n", failure[0], failure[1]);
            }
            hasErrors = true;
        }

        if (hasErrors) {
            System.exit(1);
        }

        System.out.printf("All %d CID files match their contents.%n", count);
        System.out.printf("All %d downloaded CIDs are valid.%n", count);
    }
}
