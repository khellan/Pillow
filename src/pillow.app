% pillow.app
{application, pillow, [
  {description, "Pillow, the CouchDB router and rereducer"},
  {vsn, "0.1"},
  {modules, [pillow,
             pillow_app,
             pillow_sup,
             pillow_router,
             pillow_reducer]},
  {registered, []},
  {applications, [kernel, stdlib, crypto]},
  {env, []},
  {mod, {pillow_app, []}}
]}.
