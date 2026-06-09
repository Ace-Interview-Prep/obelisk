## jenga\.frontend\.js\.package



GHCJS-compiled frontend derivation\.



*Type:*
null or package



*Default:*
` jengaLib.frontendJs config `



## jenga\.frontend\.js\.compress

Whether to compress frontend JS with brotli/gzip\.



*Type:*
boolean



*Default:*
` true `



## jenga\.frontend\.js\.compressed



Compressed frontend jsexe for jenga-asset-serve-snap\.



*Type:*
null or package



*Default:*
` assets.mkAssets optimized `



## jenga\.frontend\.js\.optimization\.enable



Whether to run closure-compiler on frontend JS\.



*Type:*
boolean



*Default:*
` true `



## jenga\.frontend\.js\.optimization\.externs



Extern files passed to closure-compiler via --externs\.



*Type:*
list of absolute path



*Default:*
` [ ] `



## jenga\.frontend\.js\.optimization\.extraFlags



Extra flags passed to closure-compiler\.



*Type:*
list of string



*Default:*
` [ ] `



## jenga\.frontend\.js\.optimization\.level



Closure-compiler optimization level\.



*Type:*
one of “BUNDLE”, “WHITESPACE_ONLY”, “SIMPLE”, “TRANSPILE_ONLY”, “ADVANCED”



*Default:*
` "ADVANCED" `



## jenga\.frontend\.js\.optimized



Closure-compiled frontend jsexe\.



*Type:*
null or package



*Default:*
` closure-compiler frontendJs `



## jenga\.frontend\.target



Frontend compilation target\.



*Type:*
one of “js”, “wasm”



*Default:*
` "wasm" `



## jenga\.frontend\.wasm\.package



WASM-compiled frontend derivation\.



*Type:*
null or package



*Default:*
` jengaLib.frontendWasm config `



## jenga\.frontend\.wasm\.compress



Whether to compress frontend WASM with brotli/gzip\.



*Type:*
boolean



*Default:*
` true `



## jenga\.frontend\.wasm\.compressed



Compressed WASM frontend for jenga-asset-serve-snap\.



*Type:*
null or package



*Default:*
` assets.mkAssets optimized `



## jenga\.frontend\.wasm\.optimization\.enable



Whether to run wasm-opt on frontend WASM\.



*Type:*
boolean



*Default:*
` true `



## jenga\.frontend\.wasm\.optimization\.extraFlags



Extra flags passed to wasm-opt\.



*Type:*
list of string



*Default:*

```
[
  "-ol"
  "2"
  "-s"
  "1"
  "--low-memory-unused"
  "--strip-dwarf"
  "--converge"
]
```



## jenga\.frontend\.wasm\.optimization\.level



wasm-opt optimization level (-O)\.



*Type:*
one of “0”, “1”, “2”, “3”, “4”, “s”, “z”



*Default:*
` "2" `



## jenga\.frontend\.wasm\.optimized



Optimized WASM frontend jsexe directory\.



*Type:*
null or package



*Default:*
` wasm-opt + post-link.mjs `



## jenga\.static\.compress



Whether to compress static assets with zopfli/gzip\.



*Type:*
boolean



*Default:*
` true `



## jenga\.static\.compressed



Hashed static assets after optional compression\. Used by overrides\.



*Type:*
null or package



*Default:*
` assets.mkAssets hashedStatic `



## jenga\.static\.path



Static assets path or derivation\.



*Type:*
null or absolute path or package



*Default:*
` null `


