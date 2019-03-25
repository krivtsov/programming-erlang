{application, sellaprime,
[{description, "The Prime Number Shop"},
  {vsn, "1.0"},
  {modules, [sellaprime_app, sellaprime_supervisor, area_server,
    prime_server, lib_primes, my_alarm_handler]},
    {registered, [area_server, prime_server, sellaprime_supervisor]},
    {application, [kernel, stdlib]},
    {mod, {sellaprime_app, []}},
    {start_phases, []}
]}.