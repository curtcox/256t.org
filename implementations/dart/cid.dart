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

class DownloadResult {
  final Uint8List content;
  final String computed;
  final bool isValid;

  DownloadResult(this.content, this.computed, this.isValid);
}

Future<DownloadResult> downloadCid(String baseUrl, String cid) async {
  final url = '${baseUrl.replaceAll(RegExp(r'/$'), '')}/$cid';
  final client = HttpClient();
  
  try {
    final request = await client.getUrl(Uri.parse(url));
    final response = await request.close();
    
    if (response.statusCode != 200) {
      throw Exception('HTTP ${response.statusCode}: ${response.reasonPhrase}');
    }
    
    final bytes = await response.expand((chunk) => chunk).toList();
    final content = Uint8List.fromList(bytes);
    final computed = computeCid(content);
    final isValid = computed == cid;
    
    return DownloadResult(content, computed, isValid);
  } finally {
    client.close();
  }
}
