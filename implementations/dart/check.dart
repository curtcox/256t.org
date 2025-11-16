import 'dart:io';

import 'cid.dart';

void main() {
  final entries = cidsDir
      .listSync()
      .whereType<File>()
      .toList()
    ..sort((a, b) => a.path.compareTo(b.path));

  var mismatches = <String>[];
  for (final file in entries) {
    final expected = computeCid(file.readAsBytesSync());
    final actual = file.uri.pathSegments.last;
    if (expected != actual) {
      mismatches.add('$actual should be $expected');
    }
  }

  if (mismatches.isNotEmpty) {
    stdout.writeln('Found CID mismatches:');
    for (final mismatch in mismatches) {
      stdout.writeln('- $mismatch');
    }
    exitCode = 1;
    return;
  }

  stdout.writeln('All ${entries.length} CID files match their contents.');
}
