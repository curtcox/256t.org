#!/usr/bin/env python3
"""
Upload and verify CID files to CloudFlare R2.

This script:
- Checks all files in the /cids directory
- Verifies if each CID exists on CloudFlare R2
- Compares content if exists and reports differences
- Uploads missing CIDs with immutable cache settings
- Verifies uploaded content and headers
- Fails if any discrepancies are found
"""

import base64
import hashlib
import os
import sys
from pathlib import Path
from typing import List, Tuple

try:
    import boto3
    from botocore.config import Config
    from botocore.exceptions import ClientError
except ImportError:
    print("Error: boto3 is required. Install with: pip install boto3")
    sys.exit(1)


def to_base64url(data: bytes) -> str:
    """Encode data to base64url format (RFC 4648 section 5)."""
    return base64.urlsafe_b64encode(data).decode().rstrip("=")


def encode_length(length: int) -> str:
    """Encode length as 8-character base64url string."""
    return to_base64url(length.to_bytes(6, "big"))


def compute_cid(content: bytes) -> str:
    """Compute the CID for given content."""
    prefix = encode_length(len(content))
    if len(content) <= 64:
        suffix = to_base64url(content)
    else:
        suffix = to_base64url(hashlib.sha512(content).digest())
    return prefix + suffix


def get_r2_client():
    """Create and return an R2 client using boto3."""
    # Get credentials from environment
    account_id = os.environ.get("CLOUDFLARE_ACCOUNT_ID")
    access_key_id = os.environ.get("R2_ACCESS_KEY_ID")
    secret_access_key = os.environ.get("R2_SECRET_ACCESS_KEY")
    bucket_name = os.environ.get("R2_BUCKET_NAME", "256t-cids")
    
    if not all([account_id, access_key_id, secret_access_key]):
        print("Error: Missing required environment variables:")
        if not account_id:
            print("  - CLOUDFLARE_ACCOUNT_ID")
        if not access_key_id:
            print("  - R2_ACCESS_KEY_ID")
        if not secret_access_key:
            print("  - R2_SECRET_ACCESS_KEY")
        sys.exit(1)
    
    # R2 endpoint format: https://<account_id>.r2.cloudflarestorage.com
    endpoint_url = f"https://{account_id}.r2.cloudflarestorage.com"
    
    s3_client = boto3.client(
        "s3",
        endpoint_url=endpoint_url,
        aws_access_key_id=access_key_id,
        aws_secret_access_key=secret_access_key,
        config=Config(signature_version="s3v4"),
        region_name="auto",
    )
    
    return s3_client, bucket_name


def check_object_exists(s3_client, bucket_name: str, key: str) -> Tuple[bool, dict]:
    """
    Check if an object exists in R2.
    
    Returns:
        (exists, metadata) - metadata is empty dict if not exists
    """
    try:
        response = s3_client.head_object(Bucket=bucket_name, Key=key)
        return True, response
    except ClientError as e:
        if e.response["Error"]["Code"] == "404":
            return False, {}
        else:
            raise


def get_object_content(s3_client, bucket_name: str, key: str) -> bytes:
    """Download and return object content from R2."""
    response = s3_client.get_object(Bucket=bucket_name, Key=key)
    return response["Body"].read()


def upload_object(s3_client, bucket_name: str, key: str, content: bytes) -> None:
    """
    Upload object to R2 with immutable cache settings.
    
    Sets:
    - Cache-Control: public, max-age=31536000, immutable
    - Content-Type: application/octet-stream
    """
    s3_client.put_object(
        Bucket=bucket_name,
        Key=key,
        Body=content,
        CacheControl="public, max-age=31536000, immutable",
        ContentType="application/octet-stream",
    )


def verify_cache_headers(metadata: dict) -> List[str]:
    """
    Verify that cache headers indicate immutable resource.
    
    Returns list of issues found, empty if all good.
    """
    issues = []
    
    cache_control = metadata.get("CacheControl", "")
    if "immutable" not in cache_control.lower():
        issues.append(f"Cache-Control missing 'immutable': {cache_control}")
    
    if "max-age=31536000" not in cache_control:
        issues.append(f"Cache-Control missing 'max-age=31536000': {cache_control}")
    
    if "public" not in cache_control.lower():
        issues.append(f"Cache-Control missing 'public': {cache_control}")
    
    return issues


def main():
    """Main function to process all CID files."""
    # Get the repository root (2 levels up from .github/scripts)
    repo_root = Path(__file__).resolve().parents[2]
    cids_dir = repo_root / "cids"
    
    if not cids_dir.exists():
        print(f"Error: CIDs directory not found: {cids_dir}")
        sys.exit(1)
    
    # Get R2 client
    try:
        s3_client, bucket_name = get_r2_client()
    except Exception as e:
        print(f"Error creating R2 client: {e}")
        sys.exit(1)
    
    # Verify bucket exists
    try:
        s3_client.head_bucket(Bucket=bucket_name)
        print(f"✓ Connected to R2 bucket: {bucket_name}")
    except ClientError as e:
        print(f"Error: Cannot access bucket '{bucket_name}': {e}")
        sys.exit(1)
    
    # Process all CID files
    cid_files = sorted([f for f in cids_dir.iterdir() if f.is_file()])
    print(f"\nProcessing {len(cid_files)} CID files...\n")
    
    errors = []
    uploaded = []
    verified = []
    
    for cid_file in cid_files:
        cid_name = cid_file.name
        content = cid_file.read_bytes()
        
        # Verify the CID matches the content
        expected_cid = compute_cid(content)
        if cid_name != expected_cid:
            error_msg = f"CID mismatch: {cid_name} should be {expected_cid}"
            print(f"✗ {error_msg}")
            errors.append(error_msg)
            continue
        
        # Check if exists on R2
        exists, metadata = check_object_exists(s3_client, bucket_name, cid_name)
        
        if exists:
            print(f"  Found: {cid_name}")
            
            # Download and verify content
            try:
                remote_content = get_object_content(s3_client, bucket_name, cid_name)
                
                if remote_content != content:
                    error_msg = f"Content mismatch for {cid_name}"
                    print(f"✗ {error_msg}")
                    print(f"  Local size: {len(content)}, Remote size: {len(remote_content)}")
                    errors.append(error_msg)
                    continue
                
                # Verify CID of downloaded content
                remote_cid = compute_cid(remote_content)
                if remote_cid != cid_name:
                    error_msg = f"CID verification failed for {cid_name}, computed: {remote_cid}"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                # Verify cache headers
                header_issues = verify_cache_headers(metadata)
                if header_issues:
                    error_msg = f"Cache header issues for {cid_name}: {', '.join(header_issues)}"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                print(f"✓ Verified: {cid_name}")
                verified.append(cid_name)
                
            except Exception as e:
                error_msg = f"Error verifying {cid_name}: {e}"
                print(f"✗ {error_msg}")
                errors.append(error_msg)
        else:
            # Upload the file
            print(f"  Missing: {cid_name}")
            try:
                upload_object(s3_client, bucket_name, cid_name, content)
                print(f"  Uploaded: {cid_name}")
                
                # Verify the upload
                exists_after, metadata_after = check_object_exists(s3_client, bucket_name, cid_name)
                if not exists_after:
                    error_msg = f"Upload verification failed: {cid_name} not found after upload"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                # Download and verify uploaded content
                uploaded_content = get_object_content(s3_client, bucket_name, cid_name)
                if uploaded_content != content:
                    error_msg = f"Uploaded content mismatch for {cid_name}"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                # Verify CID of uploaded content
                uploaded_cid = compute_cid(uploaded_content)
                if uploaded_cid != cid_name:
                    error_msg = f"Uploaded CID verification failed for {cid_name}, computed: {uploaded_cid}"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                # Verify cache headers
                header_issues = verify_cache_headers(metadata_after)
                if header_issues:
                    error_msg = f"Cache header issues after upload for {cid_name}: {', '.join(header_issues)}"
                    print(f"✗ {error_msg}")
                    errors.append(error_msg)
                    continue
                
                print(f"✓ Upload verified: {cid_name}")
                uploaded.append(cid_name)
                
            except Exception as e:
                error_msg = f"Error uploading {cid_name}: {e}"
                print(f"✗ {error_msg}")
                errors.append(error_msg)
    
    # Print summary
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Total CID files: {len(cid_files)}")
    print(f"Verified existing: {len(verified)}")
    print(f"Uploaded new: {len(uploaded)}")
    print(f"Errors: {len(errors)}")
    
    if errors:
        print("\nErrors found:")
        for error in errors:
            print(f"  - {error}")
        sys.exit(1)
    else:
        print("\n✓ All CID files successfully uploaded and verified!")
        sys.exit(0)


if __name__ == "__main__":
    main()
