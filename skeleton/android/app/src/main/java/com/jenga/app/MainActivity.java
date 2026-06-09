package com.jenga.app;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebChromeClient;
import android.webkit.WebResourceRequest;
import android.webkit.WebResourceResponse;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import java.io.IOException;
import java.io.InputStream;

/**
 * Loads the Haskell WASM frontend from bundled assets.
 *
 * The WASM binary and JS shim are in assets/frontend/.
 * We intercept requests to serve them from the asset manager
 * so the WASM app runs entirely offline-capable.
 */
public class MainActivity extends Activity {
    private WebView webView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        webView = new WebView(this);
        setContentView(webView);

        WebSettings settings = webView.getSettings();
        settings.setJavaScriptEnabled(true);
        settings.setDomStorageEnabled(true);
        settings.setAllowFileAccess(true);
        settings.setAllowContentAccess(true);

        webView.setWebChromeClient(new WebChromeClient());
        webView.setWebViewClient(new WebViewClient() {
            @Override
            public WebResourceResponse shouldInterceptRequest(WebView view, WebResourceRequest request) {
                String path = request.getUrl().getPath();
                if (path != null && path.startsWith("/")) {
                    path = path.substring(1);
                }
                if (path != null && !path.isEmpty()) {
                    String assetPath = "frontend/" + path;
                    try {
                        InputStream stream = getAssets().open(assetPath);
                        String mimeType = getMimeType(path);
                        return new WebResourceResponse(mimeType, "UTF-8", stream);
                    } catch (IOException e) {
                        // Not an asset, fall through
                    }
                }
                return super.shouldInterceptRequest(view, request);
            }
        });

        webView.loadUrl("https://appassets.androidplatform.net/index.html");
    }

    @Override
    public void onBackPressed() {
        if (webView.canGoBack()) {
            webView.goBack();
        } else {
            super.onBackPressed();
        }
    }

    private static String getMimeType(String path) {
        if (path.endsWith(".html")) return "text/html";
        if (path.endsWith(".js"))   return "application/javascript";
        if (path.endsWith(".wasm")) return "application/wasm";
        if (path.endsWith(".css"))  return "text/css";
        if (path.endsWith(".json")) return "application/json";
        if (path.endsWith(".png"))  return "image/png";
        if (path.endsWith(".svg"))  return "image/svg+xml";
        return "application/octet-stream";
    }
}
