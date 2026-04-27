# Emacs Config

## Commit message convention

Format: `<file-or-scope>: <Short description>`

Examples:
- `init-magit.el: Add cycling commands for magit status buffers`
- `init.el: gptel: Anthropic models bump (4.6 series)`
- `gitignore: /.agent-shell/`

The scope is typically the filename (without path) or a short label. Sub-scopes are separated by `: `. Description is imperative mood, capitalized, no trailing period. Single-line preferred.

One logical change per commit. Don't bundle unrelated changes together.
