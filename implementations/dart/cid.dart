import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:crypto/crypto.dart';

final Directory _scriptDir = File(Platform.script.toFilePath()).parent;
final Directory repoDir = _scriptDir.parent.parent;
final Directory cidsDir = Directory('${repoDir.path}/cids');

String _toBase64Url(Uint8List data) {
  return base64UrlEncode(data).replaceAll('=', '');
}

String encodeLength(int length) {
  final buffer = Uint8List(6);
  var value = length;
  for (var i = 5; i >= 0; i--) {
    buffer[i] = value & 0xff;
    value >>= 8;
  }
  return _toBase64Url(buffer);
}

String computeCid(List<int> content) {
  final prefix = encodeLength(content.length);
  final suffix = content.length <= 64
      ? _toBase64Url(Uint8List.fromList(content))
      : _toBase64Url(Uint8List.fromList(sha512.convert(content).bytes));
  return '$prefix$suffix';
}
