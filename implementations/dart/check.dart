import 'dart:io';
import 'dart:typed_data';

import 'cid.dart';

Future<void> main() async {
  final entries = cidsDir
      .listSync()
      .whereType<File>()
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));

  final mismatches = <String>[];
  final downloadFailures = <String>[];
  const baseUrl = 'https://256t.org';

  for (final file in entries) {
    final cid = file.uri.pathSegments.last;
    final localContent = file.readAsBytesSync();
    final expected = computeCid(localContent);
    
    // Check local CID file
    if (expected != cid) {
      mismatches.add('$cid should be $expected');
    }
    
    // Check downloaded content
    try {
      final result = await downloadCid(baseUrl, cid);
      if (!result.isValid) {
        downloadFailures.add('$cid: ${result.computed}');
      } else if (!_bytesEqual(result.content, localContent)) {
        downloadFailures.add('$cid: content mismatch with local file');
      }
    } catch (e) {
      downloadFailures.add('$cid: $e');
    }
  }

  var hasErrors = false;

  if (mismatches.isNotEmpty) {
    stdout.writeln('Found CID mismatches:');
    for (final mismatch in mismatches) {
      stdout.writeln('- $mismatch');
    }
    hasErrors = true;
  }

  if (downloadFailures.isNotEmpty) {
    stderr.writeln('Found download validation failures:');
    for (final failure in downloadFailures) {
      stderr.writeln('- $failure');
    }
    hasErrors = true;
  }

  if (hasErrors) {
    exitCode = 1;
    return;
  }

  stdout.writeln('All ${entries.length} CID files match their contents.');
  stdout.writeln('All ${entries.length} downloaded CIDs are valid.');
}

bool _bytesEqual(Uint8List a, List<int> b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    if (a[i] != b[i]) return false;
  }
  return true;
}
