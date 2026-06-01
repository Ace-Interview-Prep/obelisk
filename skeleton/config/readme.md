Configuration files are organized by visibility:

- `common/` — shared between frontend and backend (e.g. base route URL)
- `backend/` — backend-only secrets (SMTP credentials, session keys, admin email)
- `frontend/` — frontend-only config (currently unused)

At runtime, `Jenga.Configs.getConfig "common/route"` reads `config/common/route`.
Backend configs are never sent to the client.
