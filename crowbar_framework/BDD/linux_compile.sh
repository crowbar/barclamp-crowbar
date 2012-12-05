# Clean-up
rm -f erl_crash.dump
rm -f *.beam
rm -f trace_*.txt

# Compile
for f in *.erl; do
  erlc $f +debug_info
done

# Run Tests
# erl -s bdd test "crowbar" -s init stop -noshell

