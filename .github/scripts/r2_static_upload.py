#!/usr/bin/env python3
"""
R2 Static Site Upload Script for 256t.org

This script uploads static site files to the Cloudflare R2 bucket that backs 256t.org.
It uploads all files from the dist/ directory with appropriate content types and cache headers.

Usage:
    python r2_static_upload.py <dist_directory>
"""

import argparse
import sys
import logging
import subprocess
import mimetypes
from pathlib import Path

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

# Hardcoded bucket name (same as CID bucket)
BUCKET_NAME = "256t-cids"

# Initialize mimetypes
mimetypes.init()

# Add custom mime types
mimetypes.add_type('application/json', '.json')
mimetypes.add_type('text/html', '.html')
mimetypes.add_type('text/css', '.css')
mimetypes.add_type('application/javascript', '.js')
mimetypes.add_type('image/svg+xml', '.svg')


def get_content_type(file_path: Path) -> str:
    """
    Determine the content type for a file based on its extension.
    
    Args:
        file_path: Path to the file
        
    Returns:
        The MIME type string
    """
    mime_type, _ = mimetypes.guess_type(str(file_path))
    
    # Default to application/octet-stream if unknown
    if mime_type is None:
        mime_type = 'application/octet-stream'
    
    return mime_type


def run_wrangler_command(args: list) -> tuple[int, str, str]:
    """
    Run a wrangler command and return the result.
    
    Args:
        args: List of command arguments (e.g., ['r2', 'object', 'put', ...])
        
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


def upload_file_to_r2(file_path: Path, object_key: str) -> bool:
    """
    Upload a single file to R2.
    
    Args:
        file_path: Local path to the file
        object_key: The key to use in R2 (relative path from dist/)
        
    Returns:
        True if successful, False otherwise
    """
    content_type = get_content_type(file_path)
    object_path = f"{BUCKET_NAME}/{object_key}"
    
    logger.info(f"Uploading {object_key} (type: {content_type})...")
    
    # Use appropriate cache headers for static files
    # - HTML files: short cache for content updates
    # - Other files: longer cache but not immutable
    if file_path.suffix == '.html':
        cache_control = 'public, max-age=300'  # 5 minutes for HTML
    elif file_path.suffix == '.json':
        cache_control = 'public, max-age=300'  # 5 minutes for JSON
    else:
        cache_control = 'public, max-age=3600'  # 1 hour for other files
    
    returncode, stdout, stderr = run_wrangler_command([
        'r2', 'object', 'put', object_path,
        '--file', str(file_path),
        '--cache-control', cache_control,
        '--content-type', content_type,
        '--remote'
    ])
    
    if returncode != 0:
        logger.error(f"Failed to upload {object_key}: {stderr}")
        return False
    
    logger.info(f"✓ Uploaded {object_key}")
    return True


def upload_directory_to_r2(dist_dir: Path) -> int:
    """
    Upload all files from a directory to R2.
    
    Args:
        dist_dir: Path to the dist directory
        
    Returns:
        Number of failed uploads
    """
    if not dist_dir.exists() or not dist_dir.is_dir():
        logger.error(f"Directory not found or not a directory: {dist_dir}")
        return 1
    
    # Find all files recursively
    files = [f for f in dist_dir.rglob('*') if f.is_file()]
    
    logger.info(f"Found {len(files)} files to upload from {dist_dir}")
    
    failed = 0
    for file_path in files:
        # Get relative path from dist_dir to use as object key
        relative_path = file_path.relative_to(dist_dir)
        object_key = str(relative_path).replace('\\', '/')  # Ensure forward slashes
        
        if not upload_file_to_r2(file_path, object_key):
            failed += 1
    
    return failed


def main():
    parser = argparse.ArgumentParser(
        description='Upload static site files to R2 bucket using Wrangler',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('dist_dir', type=Path, help='Directory containing static files to upload')
    parser.add_argument('--debug', action='store_true',
                       help='Enable debug logging')
    
    args = parser.parse_args()
    
    if args.debug:
        logger.setLevel(logging.DEBUG)
    
    try:
        failed = upload_directory_to_r2(args.dist_dir)
        
        if failed > 0:
            logger.error(f"Failed to upload {failed} file(s)")
            sys.exit(1)
        else:
            logger.info("Successfully uploaded all files!")
            sys.exit(0)
    except Exception as e:
        logger.error(f"Error: {e}")
        sys.exit(1)


if __name__ == '__main__':
    main()
