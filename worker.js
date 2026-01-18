/**
 * Cloudflare Worker for 256t.org
 * 
 * This worker sits in front of the R2 bucket and handles:
 * 1. Serving index.html for the root path (/)
 * 2. Serving index.html for directory paths
 * 3. Proxying all other requests to R2
 */

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    let path = url.pathname;

    // Handle root path - serve index.html
    if (path === '/' || path === '') {
      path = '/index.html';
    }
    // Handle directory paths - serve index.html if it exists
    else if (path.endsWith('/')) {
      path = path + 'index.html';
    }

    // Remove leading slash for R2 key
    const key = path.startsWith('/') ? path.slice(1) : path;

    try {
      // Try to get the object from R2
      const object = await env.BUCKET.get(key);

      if (object === null) {
        // Object not found - return 404
        return new Response('Not Found', { 
          status: 404,
          headers: {
            'Content-Type': 'text/plain'
          }
        });
      }

      // Get headers from R2 object
      const headers = new Headers();
      object.writeHttpMetadata(headers);
      headers.set('etag', object.httpEtag);

      // Add CORS headers for web access
      headers.set('Access-Control-Allow-Origin', '*');
      headers.set('Access-Control-Allow-Methods', 'GET, HEAD, OPTIONS');
      headers.set('Access-Control-Allow-Headers', 'Content-Type');

      // Return the object
      return new Response(object.body, {
        headers,
      });
    } catch (error) {
      return new Response(`Error: ${error.message}`, { 
        status: 500,
        headers: {
          'Content-Type': 'text/plain'
        }
      });
    }
  },
};
