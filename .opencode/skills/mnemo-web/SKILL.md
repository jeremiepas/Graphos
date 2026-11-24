---
name: mnemo-web
description: Start the local mnemo web console for a space. Trigger when an agent or a human wants to browse the memory graph in a browser.
---

# mnemo web

Start the local web console of the space on `127.0.0.1`; it prints the URL (carrying the token of the credentials file) once.

```bash
mnemo web [--host ADDRESS] [--port PORT]
```

- One process renders **exactly one space** (there is no space switcher); a second view is a second process, not a second database.
- Graphos console → `127.0.0.1:7481` (`mnemo-graphos web`); homelab console → `127.0.0.1:7480`. The graph itself is always the single `7477`.
