if [ ! -e /usr/bin/erl ]
then
  apt-get -y install erlang-base erlang-inets
fi

# Clean-up
rm -f erl_crash.dump
rm -f *.beam

# Compile
for f in *.erl; do
  erlc $f
done

# Run Tests
erl -s bdd test "crowbar" -s init stop -noshell
