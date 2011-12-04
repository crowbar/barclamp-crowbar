REM CLEANUP
del erl_crash.dump
del *.beam

REM Compile
erlc json.erl
erlc digest_auth.erl
erlc bdd_utils.erl
erlc sc.erl
erlc bdd_webrat.erl
erlc bdd_catchall.erl
erlc bdd_selftest.erl
erlc crowbar.erl
erlc swift.erl
erlc bdd.erl

REM Self Diagnostics
erl -s bdd test "crowbar" -s init stop -noshell
