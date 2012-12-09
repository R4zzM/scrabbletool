{application, scrabbletool,
 [{description, "A practicing tool for scrabble-like games"},
  {vsn, "0.1.0"},
  {modules, [st_app, st_sup, st_worker, st_worker_sup, st_database, st_dict_parser]},
  {registered, [scrabble_tool]},
  {applications, [kernel, stdlib]},
  {mod, {st_app, [testdict.txt]}},
  {env, []}
]}.
