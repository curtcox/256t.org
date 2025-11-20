#!/usr/bin/env python3
"""
R2 Upload Script for 256t.org Content-Addressable Storage

This script uploads files to Cloudflare R2 using their CID (Content Identifier)
as the key. It uses Cloudflare's Wrangler CLI for all R2 operations.

Usage:
    python r2_upload.py <file_path> [--verify-only]
"""

import argparse
import sys
import logging
import subprocess
import tempfile
import os
from pathlib import Path

# Add the Python implementation directory to the path to import cid module
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "implementations" / "python"))

from cid import compute_cid

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

# Hardcoded bucket name
BUCKET_NAME = "256t-cids"


def run_wrangler_command(args: list) -> tuple[int, str, str]:
    """
    Run a wrangler command and return the result.
    
    Args:
        args: List of command arguments (e.g., ['r2', 'object', 'get', ...])
        
    Returns:
        Tuple of (exit_code, stdout, stderr)
    """
    cmd = ['npx', 'wrangler'] + args
    logger.debug(f"Running command: {' '.join(cmd)}")
    
    result = subprocess.run(
        cmd,
        capture_output=True,
        text=True
    )
    
    return result.returncode, result.stdout, result.stderr


def check_object_exists(cid: str) -> tuple[bool, bytes]:
    """
    Check if an object exists in R2 and return its content if it does.
    
    Args:
        cid: The CID to check
        
    Returns:
        Tuple of (exists, content) where content is bytes if exists, else empty bytes
    """
    with tempfile.NamedTemporaryFile(delete=False) as tmp_file:
        tmp_path = tmp_file.name
    
    try:
        # Try to download the object
        object_path = f"{BUCKET_NAME}/{cid}"
        returncode, stdout, stderr = run_wrangler_command([
            'r2', 'object', 'get', object_path,
            '--file', tmp_path,
            '--remote'
        ])
        
        if returncode == 0:
            # Object exists, read its content
            content = Path(tmp_path).read_bytes()
            return True, content
        else:
            # Object doesn't exist or other error
            if 'not found' in stderr.lower() or 'does not exist' in stderr.lower():
                return False, b''
            else:
                # Some other error occurred
                logger.error(f"Error checking object: {stderr}")
                raise RuntimeError(f"Failed to check object existence: {stderr}")
    finally:
        # Clean up temporary file
        if os.path.exists(tmp_path):
            os.unlink(tmp_path)


def upload_to_r2(file_path: Path) -> str:
    """
    Upload a file to R2 using its CID as the key.
    
    Args:
        file_path: Path to the file to upload
        
    Returns:
        The CID of the uploaded file
    """
    # Read file content and compute CID
    content = file_path.read_bytes()
    cid = compute_cid(content)
    
    logger.info(f"Computed CID for {file_path.name}: {cid}")
    
    # Check if object already exists and verify
    exists, remote_content = check_object_exists(cid)
    
    if exists:
        # Verify the content matches
        remote_cid = compute_cid(remote_content)
        
        if remote_cid == cid:
            logger.info(f"Object already exists with matching CID. Skipping upload.")
            return cid
        else:
            # Content CID mismatch - this is an error condition
            logger.error(f"Content CID mismatch! Expected: {cid}, Found: {remote_cid}")
            raise ValueError(f"Object exists but content does not match computed CID")
    
    # Upload the object using Wrangler
    logger.info(f"Uploading to R2...")
    object_path = f"{BUCKET_NAME}/{cid}"
    
    returncode, stdout, stderr = run_wrangler_command([
        'r2', 'object', 'put', object_path,
        '--file', str(file_path),
        '--cache-control', 'public, max-age=31536000, immutable',
        '--content-type', 'application/octet-stream',
        '--remote'
    ])
    
    if returncode != 0:
        logger.error(f"Upload failed: {stderr}")
        raise RuntimeError(f"Failed to upload to R2: {stderr}")
    
    logger.info(f"Successfully uploaded {file_path.name} to R2 as {cid}")
    return cid


def verify_r2_object(cid: str) -> bool:
    """
    Verify an object in R2 matches its CID.
    
    Verification process:
    1. Download the object from R2
    2. Compute the CID from the downloaded content
    3. Compare with the expected CID
    
    Args:
        cid: The CID to verify
        
    Returns:
        True if object is valid, False otherwise
    """
    exists, remote_content = check_object_exists(cid)
    
    if not exists:
        logger.error(f"✗ Object not found: {cid}")
        return False
    
    # Compute CID from downloaded content
    remote_cid = compute_cid(remote_content)
    
    if remote_cid == cid:
        logger.info(f"✓ Verification successful: CID matches")
        return True
    else:
        logger.error(f"✗ Content CID mismatch! Expected: {cid}, Computed: {remote_cid}")
        return False


def main():
    parser = argparse.ArgumentParser(
        description='Upload files to R2 with CID-based content addressing using Wrangler',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('file', type=Path, help='File to upload')
    parser.add_argument('--verify-only', action='store_true', 
                       help='Only verify existing object, do not upload')
    parser.add_argument('--debug', action='store_true',
                       help='Enable debug logging')
    
    args = parser.parse_args()
    
    if args.debug:
        logger.setLevel(logging.DEBUG)
    
    if not args.file.exists():
        logger.error(f"File not found: {args.file}")
        sys.exit(1)
    
    try:
        if args.verify_only:
            content = args.file.read_bytes()
            cid = compute_cid(content)
            logger.info(f"Verifying CID: {cid}")
            if verify_r2_object(cid):
                logger.info("Verification successful!")
                sys.exit(0)
            else:
                logger.error("Verification failed!")
                sys.exit(1)
        else:
            cid = upload_to_r2(args.file)
            logger.info(f"Final CID: {cid}")
            sys.exit(0)
    except Exception as e:
        logger.error(f"Error: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
