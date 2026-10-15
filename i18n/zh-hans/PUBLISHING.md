# 发布与存储

为了部署使用 CID 的内容寻址存储，本仓库提供了将文件上传到 Cloudflare R2（兼容 S3 的存储）的工具。

## R2 上传脚本

[`.github/scripts/r2_upload.py`](https://github.com/curtcox/256t.org/blob/main/.github/scripts/r2_upload.py) 脚本会将文件上传到 R2，并基于元数据进行 CID 验证：

- 将 CID 存储在对象元数据中（`Metadata={'cid': cid}`）
- 使用 `head_object()` 检查元数据来验证对象（速度快，无需下载）
- 如果元数据缺失，则回退为下载对象并计算 CID
- 不依赖 ETag（ETag 无法设置，而且对分段上传并不可靠）

详细用法、示例，以及一个演示如何为客户端覆盖 ETag 响应头的 Cloudflare Worker 代码片段，请参阅 [`.github/scripts/R2.md`](https://github.com/curtcox/256t.org/blob/main/.github/scripts/R2.md)（英文）。

---

[← 返回首页](index.html)
