## Crowbar Database (Postgresql 9.3)

Migrating to delayed_jobs for all our background processing made it
immediatly obvious that sqlite is not at all suited to handling real
concurrency once we started doing multiple jig runs on different nodes
at a time. Postgresql is more than capable of handling our forseeable
concurrency and HA use cases, and gives us lots of scope for future
optimizations and scalability.
