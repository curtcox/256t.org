import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class Generate {
    public static void main(String[] args) throws IOException {
        Files.createDirectories(CidUtil.CIDS_DIR);
        for (Path path : Files.list(CidUtil.EXAMPLES_DIR).toList()) {
            if (Files.isDirectory(path)) {
                continue;
            }
            byte[] content = CidUtil.readFile(path);
            String cid = CidUtil.computeCid(content);
            Path destination = CidUtil.CIDS_DIR.resolve(cid);
            CidUtil.writeFile(destination, content);
            System.out.printf("Wrote %s from %s%n", cid, path.getFileName());
        }
    }
}
