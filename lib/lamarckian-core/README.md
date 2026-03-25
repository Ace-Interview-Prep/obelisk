# lamarckian-core

Growth by usage was Lamarckians formerly famous idea that our strengths could be passed down through genetics.
In the same way, we hope to build landing pages that evolve by heavy user usage.

This is the platform-independent core of the Lamarckian static site library. It provides the
foundational types, template slot substitution, static widget rendering, and JS codegen helpers.
The full `lamarckian` package depends on this and adds Snap integration, MMark rendering, and
compile-time site compilers.

The Lamarckian library is a great tool for
    - Blogs            ==> Effortless Markdown Usage and integration in Reflex, an excellent static dom builder.
    - Landing Pages    ==> Routing and accurate link generation without the performance hit of SPA's. SEO-Optimized.
    - Quick Mockups    ==> Easily compatible with ClasshSS and reflex-classh for UI development,
                           but doesn't force the user to use Haskell over JS.

A neat and useful feature of Lamarckian is that it shifts its static page generation to the compile stage, meaning that
a number of errors that could occur when using static JS are actually caught by the compiler, such as the common task
of creating a DOM which rotates between a specific number of frames/sub-DOMs, we can both ensure that you avoid indexing
issues or mixing up the number of args this function takes, or even how many functions you have. You of course also
have access to the full type-safe suite of Haskell for static web development as this works with any version of GHC.

## Modules

| Module | Description |
|--------|-------------|
| `Lamarckian.Types` | Core types: `StaticWidget'`, `StaticSite`, `HtmlString`, `TemplateVars`, `HTemplateVars` |
| `Lamarckian.Render` | Run `StaticWidget'` to `ByteString` HTML, with optional template substitution |
| `Lamarckian.Template` | `{{::=name}}` slot placeholders and `unTemplate` substitution engine |
| `Lamarckian.JS` | JS function call codegen (`js2`-`js9`) and static-dynamic DOM helpers |
| `Lamarckian.MdBlock` | Composable Markdown block abstraction with deferred link resolution |

## Building

Built as a nix thunk dependency within jenga. See Haddock comments in each module for API details.
