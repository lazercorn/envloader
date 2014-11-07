{application, envloader, [
  {description, ".env environment variable loader"},
  {vsn, "0.0.1"},
  {registered, []},
  {modules, [envloader_app, envloader, envloader_sup]},
  {applications, [kernel, stdlib]},
  {env, []}
]}.
