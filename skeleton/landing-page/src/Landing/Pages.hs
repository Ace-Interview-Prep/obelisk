module Landing.Pages where

import Data.Text (Text)
import qualified Data.Text as T



-- | Root landing page HTML.
-- Replace with lamarckian TH-compiled StaticWidget' for production.
indexHtml :: Text
indexHtml = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>Welcome</title>"
  , "</head>"
  , "<body>"
  , "  <h1>Welcome to Jenga</h1>"
  , "  <nav>"
  , "    <a href=\"/about\">About</a> | "
  , "    <a href=\"/blog\">Blog</a> | "
  , "    <a href=\"/app\">App</a>"
  , "  </nav>"
  , "  <p>Edit landing-page/src/Landing/Pages.hs to customize this page.</p>"
  , "</body>"
  , "</html>"
  ]

-- | About page HTML.
aboutHtml :: Text
aboutHtml = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>About</title>"
  , "</head>"
  , "<body>"
  , "  <h1>About</h1>"
  , "  <nav>"
  , "    <a href=\"/\">Home</a> | "
  , "    <a href=\"/blog\">Blog</a> | "
  , "    <a href=\"/app\">App</a>"
  , "  </nav>"
  , "  <p>Edit this page in landing-page/src/Landing/Pages.hs.</p>"
  , "</body>"
  , "</html>"
  ]

-- | Blog index page HTML.
blogIndexHtml :: Text
blogIndexHtml = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>Blog</title>"
  , "</head>"
  , "<body>"
  , "  <h1>Blog</h1>"
  , "  <nav>"
  , "    <a href=\"/\">Home</a> | "
  , "    <a href=\"/about\">About</a> | "
  , "    <a href=\"/app\">App</a>"
  , "  </nav>"
  , "  <ul>"
  , "    <li><a href=\"/blog/first-post\">First Post</a></li>"
  , "  </ul>"
  , "</body>"
  , "</html>"
  ]

-- | Individual blog post page HTML.
blogPostHtml :: Text -> Text
blogPostHtml slug = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"utf-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "  <title>" <> slug <> "</title>"
  , "</head>"
  , "<body>"
  , "  <h1>" <> slug <> "</h1>"
  , "  <nav>"
  , "    <a href=\"/\">Home</a> | "
  , "    <a href=\"/blog\">Blog</a> | "
  , "    <a href=\"/app\">App</a>"
  , "  </nav>"
  , "  <p>Blog post: " <> slug <> "</p>"
  , "</body>"
  , "</html>"
  ]
